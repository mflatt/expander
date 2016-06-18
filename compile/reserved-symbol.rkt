#lang racket/base
(require "built-in-symbol.rkt")

;; Identifers used in the compiler's output:
(provide phase-shift-id
         dest-phase-id
         ns-id
         self-id
         syntax-literalss-id
         get-syntax-literal!-id
         bulk-binding-registry-id
         inspector-id
         deserialized-syntax-id
         set-transformer!-id
         top-level-bind!-id
         top-level-require!-id
         body-thunk-id)

(define phase-shift-id (make-built-in-symbol! 'phase))
(define dest-phase-id (make-built-in-symbol! 'dest-phase))
(define ns-id (make-built-in-symbol! 'namespace))
(define self-id (make-built-in-symbol! 'self))
(define syntax-literalss-id (make-built-in-symbol! 'syntax-literalss))
(define get-syntax-literal!-id (make-built-in-symbol! 'get-syntax-literal!))
(define bulk-binding-registry-id (make-built-in-symbol! 'bulk-binding-registry))
(define inspector-id (make-built-in-symbol! 'inspector))
(define deserialized-syntax-id (make-built-in-symbol! 'deserialized-syntax))
(define set-transformer!-id (make-built-in-symbol! 'set-transformer!))
(define top-level-bind!-id (make-built-in-symbol! 'top-level-bind!))
(define top-level-require!-id (make-built-in-symbol! 'top-level-require!))
(define body-thunk-id (make-built-in-symbol! 'body-thunk))
