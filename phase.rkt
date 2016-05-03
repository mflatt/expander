#lang racket/base

(provide phase?
         phase+
         phase-)

(define (phase? v)
  (or (not v)
      (exact-integer? v)))

(define (phase+ a b)
  (and a b (+ a b)))

(define (phase- a b)
  (- a b))
