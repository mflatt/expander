#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "syntax-to-list.rkt"
         "match.rkt"
         "datum-map.rkt")

;; Represent a compiled S-expression with source locations and properties
(provide correlate
         correlated?
         datum->correlated
         correlated-e
         correlated-cadr
         correlated-length
         correlated->list
         correlated->datum
         match-correlated)

(define (correlate stx s-exp)
  (define e
    (cond
     [(or (pair? s-exp)
          (vector? s-exp)
          (prefab-struct-key s-exp)
          (hash? s-exp)
          (box? s-exp))
      ;; Avoid pushing source locations to nested objects
      (datum->syntax #f
                     (syntax-content (datum->syntax #f s-exp))
                     stx
                     #f)]
     [else
      (datum->syntax #f s-exp stx)]))
  (define maybe-n (syntax-property stx 'inferred-name))
  (if maybe-n
      (syntax-property e 'inferred-name (if (syntax? maybe-n) 
                                            (syntax->datum maybe-n)
                                            maybe-n))
      e))

(define (correlated? e)
  (syntax? e))

(define (datum->correlated d)
  (datum->syntax #f d))

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
                     (syntax->datum e)
                     d))))

(define (match-correlated e pattern)
  (match-syntax e pattern))

;; ----------------------------------------

(module+ host
  (require "syntax-to-host-syntax.rkt")
    
  (provide correlated->host-syntax)
         
  (define (correlated->host-syntax e)
    (datum-map e (lambda (tail? d)
                   (if (syntax? d)
                       (syntax->host-syntax d)
                       d)))))
