#lang racket/base

(provide clear-resolve-cache!
         resolve-cache-get
         resolve-cache-set!)

(define cache (make-weak-box #f))

(define clear-resolve-cache!
  (case-lambda
    [(sym)
     (define c (weak-box-value cache))
     (when c
       (hash-remove! c sym))]
    [()
     (set! cache (make-weak-box (make-hasheq)))]))

(define (resolve-cache-get sym phase scopes)
  (define c (weak-box-value cache))
  (and c
       (let ([v (hash-ref c sym #f)])
         (and v
              (eqv? phase (vector-ref v 1))
              (equal? scopes (vector-ref v 0))
              (vector-ref v 2)))))

(define (resolve-cache-set! sym phase scopes b)
  (define c (weak-box-value cache))
  (cond
   [(not c)
    (clear-resolve-cache!)
    (resolve-cache-set! sym phase scopes b)]
   [else
    (hash-set! c sym (vector scopes phase b))]))
