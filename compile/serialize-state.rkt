#lang racket/base
         
(provide (struct-out serialize-state)
         make-serialize-state
         
         intern-scopes
         intern-shifted-multi-scopes
         intern-mpi-shifts
         intern-properties)

;; A `serialize-state` record is threaded through the construction of
;; a deserialization expression

(struct serialize-state (reachable-scopes       ; the set of all reachable scopes
                         bindings-intern        ; to record pruned binding tables
                         bulk-bindings-intern   ; to record pruned bulk-binding lists
                         scopes                 ; interned scope sets
                         shifted-multi-scopes   ; interned shifted multi-scope lists
                         mpi-shifts             ; interned module path index shifts
                         props                  ; map full props to previously calculated
                         interned-props))       ; intern filtered props

(define (make-serialize-state reachable-scopes)
  (serialize-state reachable-scopes
                   (make-hasheq)   ; bindings-intern
                   (make-hasheq)   ; bulk-bindings-intern
                   (make-hash)     ; scopes
                   (make-hash)     ; shifted-multi-scopes
                   (make-hasheq)   ; mpi-shifts
                   (make-hasheq)   ; props
                   (make-hash)))   ; interned-props

(define (intern-scopes scs state)
  (or (hash-ref (serialize-state-scopes state) scs #f)
      (begin
        (hash-set! (serialize-state-scopes state) scs scs)
        scs)))

(define (intern-shifted-multi-scopes sms state)
  (or (hash-ref (serialize-state-shifted-multi-scopes state) sms #f)
      (begin
        (hash-set! (serialize-state-shifted-multi-scopes state) sms sms)
        sms)))

(define (intern-mpi-shifts mpi-shifts state)
  (cond
   [(null? mpi-shifts) null]
   [else
    (define tail (intern-mpi-shifts (cdr mpi-shifts) state))
    (define tail-table (or (hash-ref (serialize-state-mpi-shifts state) tail #f)
                           (let ([ht (make-hasheq)])
                             (hash-set! (serialize-state-mpi-shifts state) tail ht)
                             ht)))
    (or (hash-ref tail-table (car mpi-shifts) #f)
        (let ([v (cons (car mpi-shifts) tail)])
          (hash-set! tail-table (car mpi-shifts) v)
          v))]))

(define (intern-properties all-props get-preserved-props state)
  (define v (hash-ref (serialize-state-props state) all-props 'no))
  (cond
   [(eq? v 'no)
    (define preserved-props (get-preserved-props))
    (define p
      (cond
       [(zero? (hash-count preserved-props)) #f]
       [(hash-ref (serialize-state-interned-props state) preserved-props #f)
        => (lambda (p) p)]
       [else
        (hash-set! (serialize-state-interned-props state) preserved-props preserved-props)
        preserved-props]))
    (hash-set! (serialize-state-props state) all-props p)
    p]
   [else v]))
