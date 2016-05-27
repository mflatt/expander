#lang racket/base
(require "set.rkt"
         (only-in "syntax.rkt"
                  syntax?
                  identifier?)
         "module-binding.rkt"
         "checked-syntax.rkt"
         (only-in "scope.rkt" add-scope)
         "namespace.rkt"
         "core.rkt"
         "phase.rkt"
         "require+provide.rkt"
         "expand-context.rkt"
         (rename-in "expand.rkt" [expand expand-in-context])
         "expand-require.rkt"
         "compile.rkt"
         "module-path.rkt"
         "linklet.rkt"
         "bulk-binding.rkt"
         "kernel.rkt"
         "utils-primitives.rkt"
         "runtime-primitives.rkt"
         "namespace-attach.rkt"
         "boot.rkt")

;; Register core forms:
(require "expand-expr.rkt"
         "expand-module.rkt"
         "expand-top-level.rkt")

;; Register core primitives:
(require "core-primitives.rkt")

;; ----------------------------------------

(define (namespace-require req [ns (current-namespace)])
  (parse-and-perform-requires! #:run? #t
                               (list (add-scope (datum->syntax #f req)
                                                (namespace-scope ns)))
                               #f ns
                               (namespace-phase ns)
                               (make-requires+provides #f)))

(define (dynamic-require mod-path sym [fail-k (lambda () (error "failed:" mod-path sym))])
  (unless (or (module-path? mod-path)
              (module-path-index? mod-path)
              (resolved-module-path? mod-path))
    (raise-argument-error 'dynamic-require
                          "(or/c module-path? module-path-index? resolved-module-path?)"
                          mod-path))
  (define ns (current-namespace))
  (define mpi
    (cond
     [(module-path? mod-path) (module-path-index-join mod-path #f)]
     [(module-path-index? mod-path) mod-path]
     [else
      (define name (resolved-module-path-name mod-path))
      (define root-name (if (pair? name) (car name) name))
      (define root-mod-path (if (path? root-name)
                                root-name
                                `(quote ,root-name)))
      (define new-mod-path (if (pair? name)
                               root-mod-path
                               `(submod ,root-mod-path ,@(cdr name))))
      (module-path-index-join new-mod-path #f)]))
  (define mod-name (module-path-index-resolve mpi #t))
  (define phase (namespace-phase ns))
  (namespace-module-instantiate! ns mpi phase)

  (define m (namespace->module ns mod-name))
  (define binding (hash-ref (hash-ref (module-provides m) 0 #hasheq())
                            sym
                            #f))
  (cond
   [(not binding) (fail-k)]
   [else
    (define ex-sym (module-binding-sym binding))
    (define ex-phase (module-binding-phase binding))
    (define m-ns (namespace->module-namespace ns
                                              (module-path-index-resolve
                                               (module-path-index-shift
                                                (module-binding-module binding)
                                                (module-self m)
                                                mpi))
                                              (phase- phase ex-phase)
                                              #:complain-on-failure? #t))
    (namespace-get-variable m-ns ex-phase ex-sym
                            (lambda ()
                              ;; Maybe syntax?
                              (define missing (gensym 'missing))
                              (define t (namespace-get-transformer m-ns ex-phase sym missing))
                              (cond
                               [(eq? t missing) (fail-k)]
                               [else
                                ;; expand in a fresh namespace
                                (define tmp-ns (make-empty-namespace ns))
                                (define name (resolved-module-path-name mod-name))
                                (define mod-path (if (path? name)
                                                     name
                                                     `(quote ,name)))
                                (namespace-require mod-path tmp-ns)
                                (eval sym tmp-ns)])))]))

(define (intro s ns)
  (if (syntax? s)
      s
      (namespace-syntax-introduce (datum->syntax #f s) ns)))

(define (expand given-s [ns (current-namespace)])
  (define s (intro given-s ns))
  (parameterize ([current-bulk-binding-fallback-registry
                  (namespace-bulk-binding-registry ns)])
    (expand-in-context s (make-expand-context ns))))

(define (compile given-s [ns (current-namespace)])
  (define s (intro given-s ns))
  (case (core-form-sym s (namespace-phase ns))
    [(module)
     (compile-module s (make-compile-context #:namespace ns))]
    [else
     (compile-top s (make-compile-context #:namespace ns))]))

(define (eval s [ns (current-namespace)])
  (parameterize ([current-bulk-binding-fallback-registry
                  (namespace-bulk-binding-registry ns)])
    (define c (if (or (compiled-top? s)
                      (linklet-directory? s))
                  s
                  (compile (expand s ns) ns)))
    (cond
     [(compiled-top? c)
      (compiled-top-run c ns)]
     [else
      (define h (linklet-directory->hash c))
      (cond
       [(hash-ref h #"" #f)
        (declare-module-from-linklet-directory! c #:namespace ns)]
       [else
        (run-top-level-from-linklet-directory c ns)])])))

(define (namespace-module-identifier [where (current-namespace)])
  (unless (or (namespace? where)
              (phase? where))
    (raise-argument-error 'namespace-module-identifier
                          (string-append "(or/c namespace? " phase?-string ")")
                          where))
  (datum->syntax (syntax-shift-phase-level core-stx
                                           (if (namespace? where)
                                               (namespace-phase where)
                                               where))
                 'module))

(define (check-module-form s)
  (unless (and (pair? (syntax-e s))
               (eq? 'module (syntax-e (car (syntax-e s)))))
    (error "not a module form:" s))
  (datum->syntax
   #f
   (cons (namespace-module-identifier)
         (cdr (syntax-e s)))))
         
;; ----------------------------------------

(define main-primitives
  (hasheq 'eval eval
          'compile compile
          'expand expand
          'dynamic-require dynamic-require
          'make-empty-namespace make-empty-namespace
          'namespace-syntax-introduce namespace-syntax-introduce
          'namespace-require namespace-require
          'namespace-module-identifier namespace-module-identifier
          'namespace-attach-module namespace-attach-module
          'namespace-attach-module-declaration namespace-attach-module-declaration))

(define (make-empty-kernel-namespace)
  (define ns (make-empty-namespace))
  (declare-core-module! ns)
  (declare-hash-based-module! '#%main main-primitives #:namespace ns)
  (declare-hash-based-module! '#%utils utils-primitives #:namespace ns)
  (declare-kernel-module! ns
                          #:eval eval
                          #:main-ids (for/set ([name (in-hash-keys main-primitives)])
                                       name))
  (for ([name (in-list runtime-instances)]
        #:unless (eq? name '#%kernel))
    (copy-racket-module! name #:namespace ns))
  (declare-reexporting-module! '#%builtin runtime-instances #:namespace ns
                               #:reexport? #f)
  ns)

;; ----------------------------------------
;; Startup

(current-namespace (make-empty-kernel-namespace))
(namespace-require ''#%kernel (current-namespace))

;; ----------------------------------------

;; Externally visible functions:
(provide boot
         
         syntax? syntax-e
         identifier?
         datum->syntax syntax->datum
         syntax-property
         
         syntax-shift-phase-level
         bound-identifier=?
         
         make-empty-namespace
         make-empty-kernel-namespace
         current-namespace
         
         namespace-syntax-introduce
         namespace-require
         dynamic-require
         namespace-module-identifier
         check-module-form
         namespace-attach-module
         namespace-attach-module-declaration
         
         expand
         compile
         eval)
