#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "binding.rkt"
         "core.rkt"
         (rename-in "expand.rkt" [expand expand-in-env])
         "compile.rkt")

;; Register core forms:
(require "expand-expr.rkt")

;; Register core primitives:
;; Enough primitives for examples...
(add-core-primitive! 'syntax-e syntax-e)
(add-core-primitive! 'datum->syntax datum->syntax)
(add-core-primitive! 'cons cons)
(add-core-primitive! 'list list)
(add-core-primitive! 'car car)
(add-core-primitive! 'cdr cdr)
(add-core-primitive! 'null? null?)
(add-core-primitive! 'map map)

;; ----------------------------------------

(define (namespace-syntax-introduce s)
  ;; The only initial bindings are in the core scope
  (add-scope s core-scope))
 
(define (expand s)
  (expand-in-env s empty-env))

(define (eval s)
  ;; Assume that `s` is compiled
  (run-time-eval s))

;; ----------------------------------------

;; Externally visible functions:
(provide syntax? syntax-e
         identifier?
         datum->syntax syntax->datum
         syntax-property
         
         bound-identifier=?
         
         namespace-syntax-introduce
         
         expand
         compile
         eval)
