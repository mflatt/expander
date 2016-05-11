#lang racket/base
(require "checked-syntax.rkt"
         (only-in "scope.rkt" add-scope)
         "namespace.rkt"
         "core.rkt"
         "require+provide.rkt"
         "expand-context.rkt"
         (rename-in "expand.rkt" [expand expand-in-context])
         "expand-require.rkt"
         "compile.rkt"
         "module-path.rkt")

;; Register core forms:
(require "expand-expr.rkt"
         "expand-module.rkt"
         "expand-top-level.rkt")

;; Register core primitives:
(require "primitives.rkt")

;; ----------------------------------------

(define (make-empty-core-namespace)
  (define ns (make-empty-namespace))
  (declare-core-module! ns)
  ns)

(define (namespace-require req [ns (current-namespace)])
  (parse-and-perform-requires! #:run? #t
                               (list (add-scope (datum->syntax #f req)
                                                (namespace-scope ns)))
                               #f ns
                               0
                               (make-requires+provides #f)))

(define (dynamic-require mod-path sym [fail-k (lambda () (error "failed:" mod-path sym))])
  (unless (module-path? mod-path)
    (raise-argument-error 'dynamic-require "module-path?" mod-path))
  (define ns (current-namespace))
  (define mod-name (resolve-module-path mod-path #f))
  (namespace-module-instantiate! ns mod-name 0)
  (define m-ns (namespace->module-namespace ns mod-name 0 #:complain-on-failure? #t))
  (namespace-get-variable m-ns 0 sym fail-k))

(define (expand s [ns (current-namespace)])
  (expand-in-context s (make-expand-context ns)))

(struct compiled-expression (s-expr)
        #:property prop:custom-write
        (lambda (c port mode)
          (fprintf port "#<compiled-expression:~.s>" (compiled-expression-s-expr c))))

(define (compile s [ns (current-namespace)])
  (compiled-expression (compile-top s (make-compile-context #:namespace ns))))

(define (eval s [ns (current-namespace)])
  (if (compiled-expression? s)
      (run-time-eval (compiled-expression-s-expr s))
      (run-time-eval (compile-top
                      (expand-in-context
                       (namespace-syntax-introduce
                        (datum->syntax #f s)
                        ns)
                       (make-expand-context ns))
                      (make-compile-context #:namespace ns)))))

;; ----------------------------------------

;; Externally visible functions:
(provide syntax? syntax-e
         identifier?
         datum->syntax syntax->datum
         syntax-property
         
         syntax-shift-phase-level
         bound-identifier=?
         
         make-empty-namespace
         make-empty-core-namespace
         current-namespace
         
         namespace-syntax-introduce
         namespace-require
         dynamic-require
         
         expand
         compile
         eval
         
         compiled-expression?)
