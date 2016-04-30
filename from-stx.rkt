#lang racket/base

(struct stx (datum phase->scopes srcloc props) #:transparent)

(struct preserved-property (v))

(define (syntax-object->stx so)
  (let loop ([so so])
    (cond
     [(syntax? so)
      (stx (loop (syntax-e so))
           #hasheqv()
           (srcloc
            (syntax-source so)
            (syntax-line so)
            (syntax-column so)
            (syntax-position so)
            (syntax-span so))
           (for/hash ([k (in-list (syntax-property-symbol-keys so))])
             (define v (syntax-property so k))
             (values k (if (syntax-property-preserved? so k)
                           (preserved-property v)
                           v))))]
     [(pair? so)
      (cons (loop (car so)) (loop (cdr so)))]
     [(vector? so)
      (vector->immutable-vector
       (for/vector #:length (vector-length so) ([v (in-vector so)])
                   (loop v)))]
     [(box? so)
      (box-immutable (loop (car so)))]
     [(prefab-struct-key so)
      => (lambda (key)
           (if (mutable-key? key)
               so
               (apply make-prefab-struct
                      (for/list ([v (in-list (cdr (vector->list (struct->vector so))))])
                        (loop v)))))]
     [else so])))

(define (mutable-key? key)
  (cond
   [(symbol? key) #t]
   [else
    (let* ([key (cdr key)] ; skip name
           [key (if (and (pair? key)
                         (number? (car key)))
                    (cdr key) ; skip count
                    key)]
           [key (if (and (pair? key)
                         (pair? (car key)))
                    (cdr key) ; skip auto-field info
                    key)])
      (cond
       [(and (pair? key) (vector (car key)))
        (cond
         [(positive? (vector-length (car key)))
          #f]
         [(null? (cdr key)) #t]
         [else (mutable-key? (cdr key))])]
       [(null? key) #t]
       [else (mutable-key? (cdr key))]))]))

(syntax-object->stx #'(foo bar))

     
