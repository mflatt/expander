#lang racket/base
(require "syntax.rkt"
         "../syntax/datum-map.rkt"
         "../common/make-match.rkt")

;; A "correlated" is the host's notion of syntax objects. We use it to
;; represent a compiled S-expression with source locations and
;; properties

(provide correlate
         correlated?
         datum->correlated
         correlated-e
         correlated-cadr
         correlated-length
         correlated->list
         correlated->datum
         correlated-property
         match-correlated)

(define (correlate src-e s-exp)
  (define e
    (cond
     [(datum-has-elements? s-exp)
      ;; Avoid pushing source locations to nested objects
      (datum->correlated (correlated-e (datum->correlated s-exp))
                         src-e)]
     [else
      (datum->correlated s-exp src-e)]))
  (define maybe-n (syntax-property src-e 'inferred-name))
  (if maybe-n
      (syntax-property e 'inferred-name maybe-n)
      e))

(define (correlated? e)
  (syntax? e))

(define (datum->correlated d [srcloc #f])
  (datum->syntax #f d srcloc))

(define (correlated-e e)
  (if (syntax? e)
      (syntax-e e)
      e))

(define (correlated-cadr e)
  (car (correlated-e (cdr (correlated-e e)))))

(define (correlated-length e)
  (define l (correlated-e e))
  (and (list? l)
       (length l)))

(define (correlated->list e)
  (if (syntax? e)
      (or (syntax->list e)
          (error 'correlate->list "not a list"))
      e))

(define (correlated->datum e)
  (datum-map e (lambda (tail? d)
                 (if (syntax? d)
                     (syntax->datum d)
                     d))))

(define correlated-property
  (case-lambda
    [(e k) (syntax-property e k)]
    [(e k v) (syntax-property e k v)]))

(define-values (match-correlated try-match-correlated)
  (make-syntax-matchers syntax? syntax-e (lambda (false str e) (error str))))

