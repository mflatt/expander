#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         "phase.rkt"
         "namespace.rkt")

(provide
 (struct-out module-binding)
 (struct-out local-binding)
 free-identifier=?
 identifier-binding-symbol
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

;; See `identifier-binding` docs for information about these fields:
(struct module-binding (module phase sym
                         nominal-module nominal-phase nominal-sym
                         nominal-require-phase)
        #:transparent)

;; Represent a local binding with a key, where the value of
;; the key is kept in a separate environment. That indirection
;; ensures that a fuly expanded program doesn't reference
;; compile-time values from local bindings, but it records that
;; the binding was local.
(struct local-binding (key))

(define (free-identifier=? a b phase)
  (define ab (resolve a phase))
  (define bb (resolve b phase))
  (cond
   [(module-binding? ab)
    (and (module-binding? bb)
         (eq? (module-binding-sym ab)
              (module-binding-sym bb))
         (eqv? (module-binding-phase ab)
               (module-binding-phase bb))
         (equal? (module-binding-module ab)
                 (module-binding-module bb)))]
   [(local-binding? ab)
    (and (local-binding? bb)
         (eq? (local-binding-key ab)
              (local-binding-key bb)))]
   [else
    (and (not ab)
         (not bb)
         (eq? (syntax-e a) (syntax-e b)))]))

(define (identifier-binding-symbol id phase)
  (define b (resolve id phase))
  (cond
   [(module-binding? b)
    (module-binding-sym b)]
   [(local-binding? b)
    (local-binding-key b)]
   [else (syntax-e id)]))

;; Helper for registering a local binding in a set of scopes:
(define (add-local-binding! id phase)
  (define key (gensym (syntax-e id)))
  (add-binding! id (local-binding key) phase)
  key)

;; ----------------------------------------

;; An expansion environment maps keys to either `variable` or a
;; compile-time value:
(define empty-env #hasheq())
(define (env-extend env key val)
  (hash-set env key val))

;; `variable` is a token to represent a binding to a run-time variable
(define variable (gensym 'variable))
(define (variable? t) (eq? t variable))

;; `unbound` is a token to represent the absence of a binding (which
;; is not always an error) as a result from `binding-lookup`; a
;; distinct token is needed so that it's distinct from all compile-time
;; values
(define unbound (gensym 'unbound))
(define (unbound? t) (eq? t unbound))

;; A subset of compile-time values are macro transformers
(define (transformer? t) (procedure? t))

;; A subset of compile-time values are primitive forms
(struct core-form (expander) #:transparent)

;; Returns `variable`, a compile-time value, or `unbound` (only
;; in the case of module-level bindings)
(define (binding-lookup b env ns phase id)
  (cond
   [(module-binding? b)
    (define m (namespace->module-namespace ns
                                           (module-binding-module b)
                                           (- phase
                                              (module-binding-phase b))))
    (namespace-get-transformer m (module-binding-phase b) (module-binding-sym b)
                               unbound)]
   [(local-binding? b)
    (define t (hash-ref env (local-binding-key b) unbound))
    (when (eq? t unbound)
      (error "identifier used out of context:" id))
    t]
   [else (error "internal error: unknown binding for lookup:" b)]))
