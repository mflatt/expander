#lang racket/base

(provide generate-lift-key)

(define (generate-lift-key)
  (gensym 'lift))
