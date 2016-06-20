#lang racket/base
(require "context.rkt")

(provide log-expand
         log-expand*
         log-expand-start)

(define-syntax log-expand*
  (syntax-rules ()
    [(_ ctx #:when guard [key arg ...] ...)
     (let ([obs (expand-context-observer ctx)])
       (when obs
         (when guard
           (call-expand-observe obs key arg ...)
           ...)))]
    [(_ ctx #:unless guard [key arg ...] ...)
     (log-expand* ctx #:when (not guard) [key arg ...] ...)]
    [(_ ctx [key arg ...] ...)
     (log-expand* ctx #:when #t [key arg ...] ...)]))

(define-syntax-rule (log-expand ctx key arg ...)
  (log-expand* ctx #:when #t [key arg ...]))

(define (call-expand-observe obs key . args)
  (apply obs (key->number key) args))

(define (log-expand-start)
  (define obs (current-expand-observe))
  (when obs
    (call-expand-observe obs 'expand-start)))

(define (key->number v)
  v)
