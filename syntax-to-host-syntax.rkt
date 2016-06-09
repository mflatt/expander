#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         (only-in racket/base [datum->syntax host:datum->syntax]))

(provide syntax->host-syntax)

(define (syntax->host-syntax v)
  (syntax-map v
              (lambda (tail? v) v)
              (lambda (orig-s d)
                (host:datum->syntax #f d (srcloc->vector (syntax-srcloc orig-s))))
              syntax-e))

(define (srcloc->vector s)
  (and s
       (vector (srcloc-source s)
               (srcloc-line s)
               (srcloc-column s)
               (srcloc-position s)
               (srcloc-span s))))
