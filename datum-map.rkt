#lang racket/base
(require "prefab.rkt")

(provide datum-map)

;; `(datum-map f v)` walks over `v`, traversing objects that
;; `datum->syntax` traverses to convert context to syntax objects.
;; 
;;  `(f tail? d)` is called to each datum `d`, where `tail?`
;;  indicates that the value is a pair/null in a `cdr` --- so that it
;;  doesn't need to be wrapped for `datum->syntax`, for example

(define (datum-map s f)
  (let loop ([tail? #f] [s s])
    (cond
     [(pair? s) (f tail? (cons (loop #f (car s))
                               (loop #t (cdr s))))]
     [(vector? s) (f #f (vector->immutable-vector
                         (for/vector #:length (vector-length s) ([e (in-vector s)])
                                     (loop #f e))))]
     [(box? s) (f #f (box-immutable (loop #f (unbox s))))]
     [(immutable-prefab-struct-key s)
      => (lambda (key)
           (f #f
              (apply make-prefab-struct
                     key
                     (for/list ([e (in-vector (struct->vector s) 1)])
                       (loop #f e)))))]
     [(and (hash? s) (immutable? s))
      (cond
       [(hash-eq? s)
        (f #f
           (for/hasheq ([(k v) (in-hash s)])
             (values k (loop #f v))))]
       [(hash-eqv? s)
        (f #f
           (for/hasheqv ([(k v) (in-hash s)])
             (values k (loop #f v))))]
       [else
        (f #f
           (for/hash ([(k v) (in-hash s)])
             (values k (loop #f v))))])]
     [(null? s) (f tail? s)]
     [else (f #f s)])))

