#lang racket/base
(require "syntax.rkt"
         "scope.rkt")

(provide make-check-no-duplicate-table
         check-no-duplicate-ids)

(define (make-check-no-duplicate-table) #hasheq())

(define (check-no-duplicate-ids ids s [ht (make-check-no-duplicate-table)])
  (let loop ([v ids] [ht ht])
    (cond
     [(identifier? v)
      (define l (hash-ref ht (syntax-e v) null))
      (for ([id (in-list l)])
        (when (bound-identifier=? id v)
          (error "duplicate binding:" v)))
      (hash-set ht (syntax-e v) (cons v l))]
     [(pair? v)
      (loop (cdr v) (loop (car v) ht))]
     [else
      ht])))

  
  
