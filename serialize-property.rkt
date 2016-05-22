#lang racket/base

(provide prop:serialize
         serialize?
         serialize-ref

         prop:serialize-fill!
         serialize-fill!?
         serialize-fill!-ref)

(define-values (prop:serialize serialize? serialize-ref)
  (make-struct-type-property 'serialize))

(define-values (prop:serialize-fill! serialize-fill!? serialize-fill!-ref)
  (make-struct-type-property 'serialize-fill!))
