#lang racket/base
(require "set.rkt"
         (only-in "syntax.rkt" syntax?)
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
         "compilation-unit.rkt"
         "bulk-binding.rkt"
         "kernel.rkt")

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
  (define mod-name 
    (cond
     [(module-path? mod-path) (resolve-module-path mod-path #f)]
     [(module-path-index? mod-path) (module-path-index-resolve mod-path #t)]
     [else mod-path]))
  (define phase (namespace-phase ns))
  (namespace-module-instantiate! ns mod-name phase)
  (define m-ns (namespace->module-namespace ns mod-name phase #:complain-on-failure? #t))
  (namespace-get-variable m-ns 0 sym
                          (lambda ()
                            ;; Maybe syntax?
                            (define missing (gensym 'missing))
                            (define t (namespace-get-transformer m-ns 0 sym missing))
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
                              (eval sym tmp-ns)]))))

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
                      (compilation-directory? s))
                  s
                  (compile (expand s ns) ns)))
    (cond
     [(compiled-top? c)
      (compiled-top-run c ns)]
     [else
      (define h (compilation-directory->hash c))
      (cond
       [(hash-ref h #"" #f)
        (declare-module-from-compilation-directory! c #:namespace ns)]
       [else
        (run-top-level-from-compilation-directory c ns)])])))

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
          'namespace-module-identifier namespace-module-identifier))

(define (make-empty-kernel-namespace)
  (define ns (make-empty-namespace))
  (declare-core-module! ns)
  (declare-hash-based-module! '#%main main-primitives #:namespace ns)
  (declare-kernel-module! ns
                          #:eval eval
                          #:main-ids (for/set ([name (in-hash-keys main-primitives)])
                                       name))
  ns)
;; ----------------------------------------

;; Externally visible functions:
(provide syntax? syntax-e
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
         
         expand
         compile
         eval)
