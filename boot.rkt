#lang racket/base
(require racket/cmdline
         "set.rkt"
         "main.rkt"
         "namespace.rkt"
         "binding.rkt"
         "primitives.rkt"
         "read-syntax.rkt"
         "module-path.rkt"
         racket/runtime-path
         (only-in syntax/modread
                  with-module-reading-parameterization)
         (only-in racket/base
                  [dynamic-require base:dynamic-require])
         (only-in "compile.rkt"
                  compile-install-primitives!)
         "cache-for-boot.rkt")

(define cache-dir #f)
(define cache-read-only? #f)
(define cache-save-only #f)
(command-line
 #:once-each
 [("-c" "--cache") dir "Save and load fomr <dir>"
  (set! cache-dir (path->complete-path dir))]
 [("-r" "--read-only") "Use cache in read-only mode"
  (set! cache-read-only? #t)]
 [("-x" "--cache-only") file "Cache only for sources listed in <file>"
  (set! cache-save-only (call-with-input-file* file read))])

(when cache-dir
  (cache-prime! cache-dir))

;; The `#lang` reader doesn't use the reimplemented module system,
;; so make sure the reader is loaded for `racket/base`:
(base:dynamic-require 'racket/base/lang/reader #f)

(define-runtime-path startup.rktl "startup.rktl")

(define boot-ns (make-empty-core-namespace))
(namespace-require ''#%core boot-ns)

(define (copy-racket-module! name #:to [to-name name]
                             #:skip [skip-syms (seteq)]
                             #:alts [alts #hasheq()])
  (define mod-name `',name)
  (define-values (vars transes) (module->exports mod-name))
  (define syms (for/list ([sym (in-list (map car (cdr (assv 0 vars))))]
                          #:unless (set-member? skip-syms sym))
                 sym))
  (define to-mpi (module-path-index-join (list 'quote to-name) #f))
  (declare-module!
   boot-ns
   (make-module #:primitive? #t
                to-mpi
                #hasheqv()
                (hasheqv 0 (for/hash ([sym (in-list syms)])
                             (values sym
                                     (make-module-binding to-mpi 0 sym))))
                0 0
                (lambda (ns phase-shift phase-level self bulk-binding-registry)
                  (when (= 0 phase-level)
                    (for ([sym (in-list syms)])
                      (namespace-set-variable! ns 0 sym
                                               (or (hash-ref alts sym #f)
                                                   (base:dynamic-require mod-name sym))))))))
  (compile-install-primitives! boot-ns (module-path-index-resolve to-mpi)))

(copy-racket-module! '#%kernel
                     #:to '#%pre-kernel
                     #:skip primitive-ids
                     #:alts (hasheq 'eval eval
                                    'compile compile
                                    'expand expand
                                    'dynamic-require dynamic-require
                                    'make-empty-namespace make-empty-namespace
                                    'namespace-syntax-introduce namespace-syntax-introduce
                                    'namespace-require namespace-require
                                    'namespace-module-identifier namespace-module-identifier))

(copy-racket-module! '#%paramz)
(copy-racket-module! '#%expobs)
(copy-racket-module! '#%foreign)
(copy-racket-module! '#%unsafe)
(copy-racket-module! '#%flfxnum)
(copy-racket-module! '#%extfl)
(copy-racket-module! '#%network)
(copy-racket-module! '#%place)
(copy-racket-module! '#%futures)

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
                (cond
                 [(and cache-dir
                       (get-cached-compiled cache-dir path
                                            (lambda ()
                                              (log-error "cached ~s" path))))
                  => eval]
                 [else
                  (log-error "compile ~s" path)
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
                    (when (and cache-dir
                               (not cache-read-only?)
                               (or (not cache-save-only)
                                   (hash-ref cache-save-only (path->string path) #f)))
                      (cache-compiled! cache-dir path c))
                    (eval c))])))

(namespace-require 'racket)
