#lang racket/base
(require '#%linklet)

;; Bounce syntax operations that are implemented by the runtime system
;; through `primitive-table`, so that the bootstrapping process
;; doesn't complain about using them.

(define kernel-primitive-table (primitive-table '#%kernel))

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (define id (hash-ref kernel-primitive-table 'id))
    ...))

(bounce read-syntax read-syntax/recursive
        datum->syntax syntax->datum syntax->list
        syntax-property-preserved? syntax-property-symbol-keys
        syntax-property syntax-span syntax-position syntax-column
        syntax-line syntax-source syntax-e syntax?)
