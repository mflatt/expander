#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         "namespace.rkt")

(provide
 (struct-out top-level-binding)
 (struct-out local-binding)
 free-identifier=?
 add-local-binding!
 
 empty-env
 env-extend
 
 variable
 (struct-out core-form)
 
 transformer?
 variable?
 unbound?
 
 binding-lookup)

;; ----------------------------------------

;; The only non-local bindings are the core forms and primitives:
(struct top-level-binding (sym))

;; Represent a local binding with a key, where the value of
;; the key is kept in a separate environment. That indirection
;; ensures that a fuly expanded program doesn't reference
;; compile-time values from local bindings, but it records that
;; the binding was local.
(struct local-binding (key))

(define (free-identifier=? a b)
  (define ab (resolve a))
  (define bb (resolve a))
  (cond
   [(top-level-binding? ab)
    (and (top-level-binding? bb)
         (eq? (top-level-binding-sym ab)
              (top-level-binding-sym bb)))]
   [(local-binding? ab)
    (and (local-binding? bb)
         (eq? (local-binding-key ab)
              (local-binding-key bb)))]))

;; Helper for registering a local binding in a set of scopes:
(define (add-local-binding! id)
  (define key (gensym (syntax-e id)))
  (add-binding! id (local-binding key))
  key)

;; ----------------------------------------

;; An expansion environment maps keys to either `variable` or a
;; compile-time value:
(define empty-env #hasheq())
(define (env-extend env key val)
  (hash-set env key val))

;; `variable` is a token to represent a binding to a run-time variable
(define variable (gensym))
(define (variable? t) (eq? t variable))

;; `unbound` is a token to represent the absence of a binding (which
;; is not always an error) as a result from `binding-lookup`; a
;; distinct token is needed so that it's distinct from all compile-time
;; values
(define unbound (gensym))
(define (unbound? t) (eq? t unbound))

;; A subset of compile-time values are macro transformers
(define (transformer? t) (procedure? t))

;; A subset of compile-time values are primitive forms
(struct core-form (expander) #:transparent)

;; Returns `variable`, a compile-time value, or `unbound` (only
;; in the case of top-level bindings)
(define (binding-lookup b env ns id)
  (cond
   [(top-level-binding? b)
    (namespace-get-transformer ns (top-level-binding-sym b) unbound)]
   [(local-binding? b)
    (define t (hash-ref env (local-binding-key b) unbound))
    (when (eq? t unbound)
      (error "identifier used out of context:" id))
    t]
   [else (error "internal error: unknown binding for lookup:" b)]))
