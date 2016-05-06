#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "namespace.rkt"
         "core.rkt"
         "require+provide.rkt"
         "expand-context.rkt"
         (rename-in "expand.rkt" [expand expand-in-context])
         "expand-require.rkt"
         (rename-in "compile.rkt" [compile compile-in-namespace]))

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

(define (namespace-require req ns)
  (parse-and-perform-requires! #:run? #t
                               (list (add-scope (datum->syntax #f req)
                                                (namespace-scope ns)))
                               #f ns
                               0
                               (make-requires+provides)))

(define (expand s [ns (current-namespace)])
  (expand-in-context s (make-expand-context ns)))

(struct compiled-expression (s-expr)
        #:property prop:custom-write
        (lambda (c port mode)
          (fprintf port "#<compiled-expression:~.s>" (compiled-expression-s-expr c))))

(define (compile s [ns (current-namespace)])
  (compiled-expression (compile-in-namespace s ns)))

(define (eval s [ns (current-namespace)])
  (if (compiled-expression? s)
      (run-time-eval (compiled-expression-s-expr s))
      (run-time-eval (compile-in-namespace
                      (expand-in-context
                       (namespace-syntax-introduce
                        (datum->syntax #f s)
                        ns)
                       (make-expand-context ns))
                      ns))))

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
         
         expand
         compile
         eval
         
         compiled-expression?)
