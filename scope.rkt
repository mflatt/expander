#lang racket/base
(require racket/set
         racket/list
         "syntax.rkt")

(provide new-scope
         add-scope
         flip-scope
         
         add-binding!
         resolve)

;; A scope represents a distinct "dimension" of binding.
;; Scope identity is `eq?` identity.

(struct scope ())

(define (new-scope) (scope))

;; Add or flip a scope everywehere (i.e., including nested syntax)
(define (adjust-scope s/e sc op)
  (cond
   [(syntax? s/e) (struct-copy syntax s/e
                               [e (adjust-scope (syntax-e s/e) sc op)]
                               [scopes (op (syntax-scopes s/e) sc)])]
   [(list? s/e) (map (lambda (s) (adjust-scope s sc op)) s/e)]
   [else s/e]))

(define (add-scope s sc)
  (adjust-scope s sc set-add))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (adjust-scope s sc set-flip))

;; ----------------------------------------

;; Global table of bindings
(define all-bindings
  (make-hash))

(define (add-binding! id binding)
  (unless (identifier? id)
    (raise-argument-error 'resolve "identifier?" id))
  (hash-set! all-bindings id binding))

;; Finds the binding for a given identifier; returns #f if the
;; identifier is unbound
(define (resolve id)
  (define candidate-ids (find-all-matching-bindings id))
  (cond
   [(pair? candidate-ids)
    (define max-candidate-id
      (argmax (lambda (c-id) (set-count (syntax-scopes c-id)))
              candidate-ids))
    (check-unambiguous max-candidate-id candidate-ids id)
    (hash-ref all-bindings max-candidate-id)]
   [else #f]))

;; Find all candidiate bindings for `id` as the ones with
;; a subset of the scopes of `id`
(define (find-all-matching-bindings id)
  (for/list ([(c-id) (in-hash-keys all-bindings)]
             #:when (and (eq? (syntax-e c-id) (syntax-e id))
                         (subset? (syntax-scopes c-id) (syntax-scopes id))))
    c-id))

;; Check that the binding with the biggest scope set is a superset
;; of all the others
(define (check-unambiguous max-candidate-id candidate-ids error-id)
  (for ([c-id (in-list candidate-ids)])
    (unless (subset? (syntax-scopes c-id)
                     (syntax-scopes max-candidate-id))
      (error "ambiguous:" error-id))))

