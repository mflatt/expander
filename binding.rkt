#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         "phase.rkt"
         "namespace.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "module-path.rkt")

(provide
 (struct-out binding)
 (struct-out module-binding)
 make-module-binding
 (struct-out local-binding)

 free-identifier=?
 same-binding?
 identifier-binding
 identifier-binding-symbol
 
 add-local-binding!
 
 maybe-install-free=id!
 binding-set-free=id

 empty-env
 env-extend
 
 variable
 (struct-out core-form)
 
 transformer? transformer->procedure
 variable?
 
 binding-lookup
 
 resolve+shift
 syntax-module-path-index-shift
 binding-module-path-index-shift)

;; ----------------------------------------

(struct binding (frame-id   ; used to trigger use-site scopes
                 free=id))  ; `free-identifier=?` equivalence via a rename-transformer binding

;; See `identifier-binding` docs for information about these fields:
(struct module-binding binding (module phase sym
                                 nominal-module nominal-phase nominal-sym
                                 nominal-require-phase)
        #:transparent)

(define (make-module-binding module phase sym
                             #:nominal-module [nominal-module module]
                             #:nominal-phase [nominal-phase phase]
                             #:nominal-sym [nominal-sym sym]
                             #:nominal-require-phase [nominal-require-phase 0]
                             #:frame-id [frame-id #f])
  (module-binding frame-id
                  #f
                  module phase sym
                  nominal-module nominal-phase nominal-sym
                  nominal-require-phase))

;; Represent a local binding with a key, where the value of
;; the key is kept in a separate environment. That indirection
;; ensures that a fuly expanded program doesn't reference
;; compile-time values from local bindings, but it records that
;; the binding was local. The `frame-id` field is used to
;; trigger use-site scopes as needed
(struct local-binding binding (key))

(define (free-identifier=? a b phase)
  (define ab (resolve+shift a phase))
  (define bb (resolve+shift b phase))
  (cond
   [(or (not ab) (not bb))
    (and (not ab)
         (not bb)
         (eq? (syntax-e a) (syntax-e b)))]
   [else
    (same-binding? ab bb)]))

(define (same-binding? ab bb)
  (cond
   [(module-binding? ab)
    (and (module-binding? bb)
         (eq? (module-binding-sym ab)
              (module-binding-sym bb))
         (eqv? (module-binding-phase ab)
               (module-binding-phase bb))
         (eq? (module-path-index-resolve (module-binding-module ab))
              (module-path-index-resolve (module-binding-module bb))))]
   [(local-binding? ab)
    (and (local-binding? bb)
         (eq? (local-binding-key ab)
              (local-binding-key bb)))]
   [else (error "bad binding")]))

(define (identifier-binding-symbol id phase)
  (define b (resolve id phase))
  (cond
   [(module-binding? b)
    (module-binding-sym b)]
   [(local-binding? b)
    (local-binding-key b)]
   [else (syntax-e id)]))

(define (identifier-binding id phase)
  (define b (resolve+shift id phase))
  (cond
   [(module-binding? b)
     (list (module-binding-module b)
           (module-binding-sym b)
           (module-binding-nominal-module b)
           (module-binding-nominal-sym b)
           (module-binding-phase b)
           (module-binding-nominal-require-phase b)
           (module-binding-nominal-phase b))]
   [(local-binding? b)
    'lexical]
   [else #f]))

;; Helper for registering a local binding in a set of scopes:
(define (add-local-binding! id phase #:frame-id [frame-id #f])
  (define key (gensym (syntax-e id)))
  (add-binding! id (local-binding frame-id #f key) phase)
  key)


;; ----------------------------------------

(define (maybe-install-free=id! val id phase)
  (when (rename-transformer? val)
    (define free=id (rename-transformer-target val))
    (unless (syntax-property free=id 'not-free-identifier=?)
      (define b (resolve+shift id phase #:exactly? #t #:immediate? #t))
      (add-binding! id (binding-set-free=id b free=id) phase))))

;; Helper to add a `free-identifier=?` equivance to a binding
(define (binding-set-free=id b free=id)
  (cond
   [(module-binding? b) (struct-copy module-binding b
                                     [free=id #:parent binding free=id])]
   [(local-binding? b) (struct-copy local-binding b
                                    [free=id #:parent binding free=id])]
   [else (error "bad binding for free=id:" b)]))

;; ----------------------------------------

;; An expansion environment maps keys to either `variable` or a
;; compile-time value:
(define empty-env #hasheq())
(define (env-extend env key val)
  (hash-set env key val))

;; `variable` is a token to represent a binding to a run-time variable
(define variable (gensym 'variable))
(define (variable? t) (eq? t variable))

;; `missing` is a token to represent the absence of a binding; a
;; distinct token is needed so that it's distinct from all compile-time
;; values
(define missing (gensym 'missing))
(define (missing? t) (eq? t missing))

;; A subset of compile-time values are macro transformers
(define (transformer? t) (or (procedure? t)
                             (set!-transformer? t)
                             (rename-transformer? t)))
(define (transformer->procedure t)
  (cond
   [(set!-transformer? t) (set!-transformer-procedure t)]
   [(rename-transformer? t) (lambda (s)
                              (if (identifier? s)
                                  (rename-transformer-target t)
                                  (datum->syntax
                                   s
                                   (cons (rename-transformer-target t)
                                         (cdr (syntax-e s)))
                                   s
                                   s)))]
   [else t]))

;; A subset of compile-time values are primitive forms
(struct core-form (expander) #:transparent)

;; Returns `variable` or a compile-time value
(define (binding-lookup b env lift-envs ns phase id)
  (cond
   [(module-binding? b)
    (define at-phase (- phase (module-binding-phase b)))
    (define m (namespace->module-namespace ns
                                           (module-path-index-resolve
                                            (module-binding-module b))
                                           at-phase))
    (unless m
      (error "namespace mismatch: cannot locate module"
             (module-binding-module b)
             at-phase))
    (namespace-get-transformer m (module-binding-phase b) (module-binding-sym b)
                               variable)]
   [(local-binding? b)
    (define t (hash-ref env (local-binding-key b) missing))
    (cond
     [(eq? t missing)
      (or
       ;; check in lift envs, if any
       (for/or ([lift-env (in-list lift-envs)])
         (hash-ref (unbox lift-env) (local-binding-key b) #f))
       (error "identifier used out of context:" id))]
     [else t])]
   [else (error "internal error: unknown binding for lookup:" b)]))

; ----------------------------------------


;; Adjust `s` (recursively) so that if `resolve+shift` would
;;  report `form-mpi`, the same operation on the result will
;;  report `to-mpi`
(define (syntax-module-path-index-shift s from-mpi to-mpi)
  (if (eq? from-mpi to-mpi)
      s
      (let ([shift (cons from-mpi to-mpi)])
        (syntax-map s
                    (lambda (tail? d) d)
                    (lambda (s d)
                      (struct-copy syntax s
                                   [e d]
                                   [mpi-shifts
                                    (cons shift (syntax-mpi-shifts s))]))))))

;; Use `resolve` instead of `resolve+shift` when the module of a
;; module binding is relevant or when `free-identifier=?` equivalences
;; (as installed by a binding to a rename transfomer) are relevant;
;; module path index shifts attached to `s` are taken into account in
;; the result
(define (resolve+shift s phase
                       #:exactly? [exactly? #f]
                       #:immediate? [immediate? exactly?])
  (define immediate-b (resolve s phase #:exactly? exactly?))
  (define b (if (and immediate-b
                     (not immediate?)
                     (binding-free=id immediate-b))
                (resolve+shift (binding-free=id immediate-b) phase
                               #:exactly? exactly?)
                immediate-b))
  (cond
   [(module-binding? b)
    (define mpi-shifts (syntax-mpi-shifts s))
    (cond
     [(null? mpi-shifts)
      b]
     [else
      (struct-copy module-binding b
                   [module (apply-shifts (module-binding-module b) mpi-shifts)]
                   [nominal-module (apply-shifts (module-binding-nominal-module b) mpi-shifts)])])]
   [else b]))

;; Apply accumulated module path index shifts
(define (apply-shifts mpi shifts)
  (cond
   [(null? shifts) mpi]
   [else
    (define shifted-mpi (apply-shifts mpi (cdr shifts)))
    (module-path-index-shift shifted-mpi (caar shifts) (cdar shifts))]))

;; Apply a single shift to a single binding
(define (binding-module-path-index-shift b from-mpi to-mpi)
  (cond
   [(module-binding? b)
    (struct-copy module-binding b
                 [module (module-path-index-shift (module-binding-module b)
                                                  from-mpi
                                                  to-mpi)]
                 [nominal-module (module-path-index-shift (module-binding-nominal-module b)
                                                          from-mpi
                                                          to-mpi)])]
   [else b]))
