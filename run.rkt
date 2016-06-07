#lang racket/base
(require racket/cmdline
         "set.rkt"
         "main.rkt"
         "namespace.rkt"
         "binding.rkt"
         "read-syntax.rkt"
         "module-path.rkt"
         "module-read.rkt"
         (only-in racket/base
                  [dynamic-require base:dynamic-require])
         "kernel.rkt"
         "run-cache.rkt"
         "runtime-primitives.rkt"
         "linklet.rkt"
         "status.rkt"
         "extract.rkt")

(define extract? #f)
(define cache-dir #f)
(define cache-read-only? #f)
(define cache-save-only #f)
(define cache-skip-first? #f)
(define time-expand? #f)
(define boot-module (path->complete-path "main.rkt"))
(define load-file #f)
(command-line
 #:once-each
 [("-x" "--extract") "Extract bootstrap linklets"
  (set! extract? #t)]
 [("-c" "--cache") dir "Save and load from <dir>"
  (set! cache-dir (path->complete-path dir))]
 [("-r" "--read-only") "Use cache in read-only mode"
  (set! cache-read-only? #t)]
 [("-y" "--cache-only") file "Cache only for sources listed in <file>"
  (set! cache-save-only (call-with-input-file* file read))]
 [("-i" "--skip-initial") "Don't use cache for the initial load"
  (set! cache-skip-first? #t)]
 [("-s" "--s-expr") "Compile to S-expression instead of bytecode"
  (linklet-compile-to-s-expr #t)]
 [("--time") "Time re-expansion"
  (set! time-expand? #t)]
 #:once-any
 [("-t") file "Load specified file"
  (set! boot-module (path->complete-path file))]
 [("-l") lib "Load specified library"
  (set! boot-module (string->symbol lib))]
 [("-f") file "Load non-module file in `racket/base` namespace"
  (set! boot-module 'racket/base)
  (set! load-file file)])

(define cache (make-cache cache-dir))

;; The `#lang` reader doesn't use the reimplemented module system,
;; so make sure the reader is loaded for `racket/base` (before
;; `boot` sets handlers):
(base:dynamic-require 'racket/base/lang/reader #f)

;; Install handlers:
(boot)

;; Avoid use of ".zo" files:
(use-compiled-file-paths null)

;; Replace the load handler to stash compiled modules in the cache
;; and/or load them from the cache
(define orig-load (current-load))
(current-load (lambda (path expected-module)
                (cond
                 [expected-module
                  (let loop ()
                    (cond
                     [(and cache
                           (not cache-skip-first?)
                           (get-cached-compiled cache path
                                                (lambda ()
                                                  (when cache-dir
                                                    (log-status "cached ~s" path)))))
                      => eval]
                     [else
                      (log-status "compile ~s" path)
                      (set! cache-skip-first? #f)
                      (with-handlers ([exn:fail? (lambda (exn)
                                                   (log-status "...during ~s..." path)
                                                   (raise exn))])
                        (define s
                          (call-with-input-file*
                           path
                           (lambda (i)
                             (port-count-lines! i)
                             (with-module-reading-parameterization
                                 (lambda ()
                                   (check-module-form
                                    (read-syntax (object-name i) i)
                                    path))))))
                        (define c (compile s))
                        (when time-expand?
                          ;; Re-expanding avoids timing load of required modules
                          (time (expand s)))
                        (cond
                         [(and cache
                               (not cache-read-only?)
                               (or (not cache-save-only)
                                   (hash-ref cache-save-only (path->string path) #f)))
                          (cache-compiled! cache path c)
                          (loop)]
                         [else (eval c)]))]))]
                 [else (orig-load path #f)])))

;; Load and run the requested module
(namespace-require boot-module)

(when extract?
  ;; Extract a bootstrapping slice of the requested module
  (extract boot-module cache))

(when load-file
  (load load-file))
