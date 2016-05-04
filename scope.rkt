#lang racket/base
(require racket/set
         "syntax.rkt")

(provide new-scope
         add-scope
         flip-scope
         
         add-binding!
         resolve)

;; A scope represents a distinct "dimension" of binding.

(struct scope ())

(define (new-scope)
  ;; scope identity is `eq?` identity
  (scope))

;; Add or flip a scope --- recurs to nested syntax
(define (apply-scope s sc op)
  (cond
   [(syntax? s) (struct-copy syntax s
                             [e (apply-scope (syntax-e s) sc op)]
                             [scopes (op (syntax-scopes s) sc)])]
   [(pair? s) (cons (apply-scope (car s) sc op)
                    (apply-scope (cdr s) sc op))]
   [else s]))

(define (add-scope s sc)
  (apply-scope s sc set-add))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (apply-scope s sc set-flip))

;; ----------------------------------------

;; Global table of bindings
(define all-bindings
  (make-hash))

(define (add-binding! id binding)
  (unless (identifier? id)
    (raise-argument-error 'resolve "identifier?" id))
  (hash-set! all-bindings id binding))

(define (resolve id)
  (unless (identifier? id)
    (raise-argument-error 'resolve "identifier?" id))
  (define candidate-ids
    (for/list ([(c-id) (in-hash-keys all-bindings)]
               #:when (and (eq? (syntax-e c-id) (syntax-e id))
                           (subset? (syntax-scopes c-id) (syntax-scopes id))))
      c-id))
  (define max-candidate-id
    (and (pair? candidate-ids)
         (car
          (sort candidate-ids >
                #:key (lambda (c-id) (set-count (syntax-scopes c-id)))))))
  (cond
   [max-candidate-id
    (for ([c-id (in-list candidate-ids)])
      (unless (subset? (syntax-scopes c-id)
                       (syntax-scopes max-candidate-id))
        (error "ambiguous:" id)))
    (hash-ref all-bindings max-candidate-id)]
   [else #f]))
