#lang racket/base
(require (for-syntax racket/base))

;; Lightweight variant of sets

(provide set seteq seteqv
         set-empty?
         set-member?
         set-count
         set-add
         set-remove
         set-first
         subset?
         set-subtract
         set->list
         for/set
         in-set)

(define set
  (case-lambda
    [() #hash()]
    [l (for/fold ([s #hash()]) ([e (in-list l)])
         (hash-set s e #t))]))
(define seteq
  (case-lambda
    [() #hasheq()]
    [l (for/fold ([s #hasheq()]) ([e (in-list l)])
         (hash-set s e #t))]))
(define (seteqv) #hasheqv())

(define (set-empty? s) (zero? (hash-count s)))
(define (set-member? s e) (hash-ref s e #f))
(define (set-count s) (hash-count s))

(define (set-add s e) (hash-set s e #t))
(define (set-remove s e) (hash-remove s e))
(define (set-first s) (hash-iterate-key s (hash-iterate-first s)))

(define (subset? s1 s2)
  (cond
   [((hash-count s1) . <= . (hash-count s2))
    (for/and ([k (in-hash-keys s1)])
      (hash-ref s2 k #f))]
   [else #f]))

(define (set-subtract s1 s2)
  (for/fold ([s1 s1]) ([k (in-hash-keys s1)])
    (hash-remove s1 k)))

(define (set->list s)
  (for/list ([k (in-hash-keys s)])
    k))

(define-syntax-rule (for/set bindings body ...)
  (for/hash bindings (values
                      (let ()
                        body ...)
                      #t)))

(define-syntax in-set (make-rename-transformer #'in-hash-keys))
