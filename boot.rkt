#lang racket/base
(require racket/cmdline
         "set.rkt"
         "main.rkt"
         "namespace.rkt"
         "binding.rkt"
         "read-syntax.rkt"
         "module-path.rkt"
         racket/runtime-path
         (only-in syntax/modread
                  with-module-reading-parameterization)
         (only-in racket/base
                  [dynamic-require base:dynamic-require])
         "kernel.rkt"
         "cache-for-boot.rkt"
         "runtime-primitives.rkt"
         "compilation-unit.rkt")

(define cache-dir #f)
(define cache-read-only? #f)
(define cache-save-only #f)
(define cache-skip-first? #f)
(define time-expand? #f)
(define boot-module 'racket)
(command-line
 #:once-each
 [("-c" "--cache") dir "Save and load fomr <dir>"
  (set! cache-dir (path->complete-path dir))]
 [("-r" "--read-only") "Use cache in read-only mode"
  (set! cache-read-only? #t)]
 [("-x" "--cache-only") file "Cache only for sources listed in <file>"
  (set! cache-save-only (call-with-input-file* file read))]
 [("-i" "--skip-initial") "Don't use cache for the initial load"
  (set! cache-skip-first? #t)]
 [("-s" "--s-expr") "Compile to S-expression instead of bytecode"
  (compilation-unit-compile-to-s-expr #t)]
 [("--time") "Time re-expansion"
  (set! time-expand? #t)]
 #:once-any
 [("-t") file "Load specified file"
  (set! boot-module (path->complete-path file))]
 [("-l") lib "Load specified library"
  (set! boot-module (string->symbol lib))])

(define cache (and cache-dir (make-cache cache-dir)))

;; The `#lang` reader doesn't use the reimplemented module system,
;; so make sure the reader is loaded for `racket/base`:
(base:dynamic-require 'racket/base/lang/reader #f)

(define-runtime-path startup.rktl "startup.rktl")

(define boot-ns (make-empty-kernel-namespace))
(namespace-require ''#%kernel boot-ns)

(for ([name (in-list runtime-instances)]
      #:unless (eq? name '#%kernel))
  (copy-racket-module! name #:namespace boot-ns))

(current-namespace boot-ns)

(define (eval-syntax s)
  (eval (compile (expand s))))

(define (eval-s-expr e)
  (eval-syntax (namespace-syntax-introduce (datum->syntax #f e))))

(call-with-input-file*
 startup.rktl
 (lambda (i)
   (port-count-lines! i)
   (for ([v (in-port (lambda (i) (read-syntax (object-name i) i)) i)])
     (eval-s-expr v))))

((dynamic-require ''#%boot 'boot))

(use-compiled-file-paths null)
(current-eval (lambda (s)
                (if (syntax? s)
                    (eval-syntax s)
                    (eval-s-expr s))))
(current-load (lambda (path expected-module)
                (let loop ()
                  (cond
                   [(and cache
                         (not cache-skip-first?)
                         (get-cached-compiled cache path
                                              (lambda ()
                                                (log-error "cached ~s" path))))
                    => eval]
                   [else
                    (log-error "compile ~s" path)
                    (set! cache-skip-first? #f)
                    (with-handlers ([exn:fail? (lambda (exn)
                                                 (log-error "...during ~s..." path)
                                                 (raise exn))])
                      (define s
                        (call-with-input-file*
                         path
                         (lambda (i)
                           (port-count-lines! i)
                           (with-module-reading-parameterization
                               (lambda ()
                                 (check-module-form
                                  (read-syntax (object-name i) i)))))))
                      (define c (compile (expand s)))
                      (when time-expand?
                        (time (expand s)))
                      (cond
                       [(and cache
                             (not cache-read-only?)
                             (or (not cache-save-only)
                                 (hash-ref cache-save-only (path->string path) #f)))
                        (cache-compiled! cache path c)
                        (loop)]
                       [else (eval c)]))]))))

(namespace-require boot-module)
