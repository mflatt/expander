#lang racket/base

(provide syntax-tainted?
         syntax-arm
         syntax-protect
         syntax-disarm
         syntax-rearm
         syntax-taint)

;; No-ops, for now

(define (syntax-tainted? stx)
  #f)

(define (syntax-arm stx [inspector #f] [use-mode? #f])
  stx)

(define (syntax-protect stx)
  (syntax-arm stx #f #t))

(define (syntax-disarm stx inspector)
  stx)

(define (syntax-rearm stx from-stx [use-mode? #f])
  stx)

(define (syntax-taint stx)
  stx)


         
