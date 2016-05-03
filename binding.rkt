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
  (unless (identifier? a)
    (raise-argument-error 'free-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'free-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'free-identifier=? "phase?" phase))
  (define ab (resolve a phase))
  (define bb (resolve a phase))
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
              (local-binding-key bb)))]))

;; Helper for registering a local binding in a set of scopes:
(define (add-local-binding! id phase)
  (define key (gensym))
  (add-binding! id (local-binding key) phase)
  key)

;; ----------------------------------------

;; An expansion environment maps keys to either `variable` or a
;; compile-time value:
(define empty-env #hasheq())
(define (env-extend env key val)
  (hash-set env key val))

(define variable (gensym))
(define (variable? t) (eq? t variable))

(define unbound (gensym))
(define (unbound? t) (eq? t unbound))

;; The subset of compile-time values that are macro transformers:
(define (transformer? t) (procedure? t))

;; The subset of compile-time values that are primitive forms:
(struct core-form (expander) #:transparent)

;; Returns `variable`, a compile-time value, or `unbound` (for
;; module-level bindings):
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
