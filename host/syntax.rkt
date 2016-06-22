#lang racket/base

;; Bounce syntax operations that are implemented by the runtime system
;; through `get-primitive-instance`, so that the bootstrapping process
;; doesn't complain about using them.

(define (get-primitive-instance name)
  `(quote ,name))
(define (instance-variable-value i sym)
  (dynamic-require i sym))

(define kernel-instance (get-primitive-instance '#%kernel))

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (define id (instance-variable-value kernel-instance 'id))
    ...))

(bounce read-syntax read-syntax/recursive
        datum->syntax syntax->datum syntax->list
        syntax-property-preserved? syntax-property-symbol-keys
        syntax-property syntax-span syntax-position syntax-column
        syntax-line syntax-source syntax-e syntax?)
