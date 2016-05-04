#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "namespace.rkt"
         "core.rkt"
         "expand-context.rkt"
         (rename-in "expand.rkt" [expand expand-in-context])
         (rename-in "compile.rkt" [compile compile-in-namespace]))

;; Register core forms:
(require "expand-expr.rkt"
         "expand-top-level.rkt")

;; Register core primitives:
;; This list will need to be a lot longer...
(add-core-primitive! 'syntax-e syntax-e)
(add-core-primitive! 'datum->syntax datum->syntax)
(add-core-primitive! 'cons cons)
(add-core-primitive! 'list list)
(add-core-primitive! 'car car)
(add-core-primitive! 'cdr cdr)
(add-core-primitive! 'null? null?)
(add-core-primitive! 'values values)

;; Fill in the (only) namespace, which ties the loop
;; between binding the expander 
(declare-core-top-level! (current-namespace))

;; ----------------------------------------

(define (namespace-syntax-introduce s)
  ;; All top-level bindings are in the core scope:
  (add-scope s core-scope))
 
(define (expand s)
  (expand-in-context s (make-expand-context (current-namespace))))

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
         
         bound-identifier=?
         
         current-namespace
         
         namespace-syntax-introduce
         
         expand
         compile
         eval
         
         compiled-expression?)
