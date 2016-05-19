#lang racket/base
(require "set.rkt"
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "env.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "core.rkt"
         "rename-trans.rkt"
         "lift-context.rkt"
         "require+provide.rkt"
         "module-path.rkt"
         "namespace.rkt"
         "contract.rkt"
         "debug.rkt")

(provide get-current-expand-context
         flip-introduction-scopes
         
         syntax-transforming?
         syntax-transforming-with-lifts?
         syntax-transforming-module-expression?
         syntax-local-transforming-module-provides?
         
         syntax-local-context
         syntax-local-introduce
         syntax-local-identifier-as-binding
         syntax-local-phase-level
         syntax-local-name

         make-syntax-introducer
         make-syntax-delta-introducer
         
         syntax-local-value
         syntax-local-value/immediate
         
         syntax-local-lift-expression
         syntax-local-lift-values-expression
         syntax-local-lift-context
         
         syntax-local-lift-module
         
         syntax-local-lift-require
         syntax-local-lift-provide
         syntax-local-lift-module-end-declaration
         
         syntax-local-module-defined-identifiers
         syntax-local-module-required-identifiers
         syntax-local-module-exports
         syntax-local-submodules
         
         syntax-local-get-shadower)

;; ----------------------------------------

(define (get-current-expand-context who)
  (or (current-expand-context)
      (error who "not currently expanding")))

(define (flip-introduction-scopes s ctx)
  (flip-scopes s (expand-context-current-introduction-scopes ctx)))

;; ----------------------------------------

(define (syntax-transforming?)
  (and (current-expand-context) #t))

(define (syntax-transforming-with-lifts?)
  (define ctx (current-expand-context))
  (and ctx
       (expand-context-lifts ctx)
       #t))

(define (syntax-transforming-module-expression?)
  (define ctx (current-expand-context))
  (and ctx
       (expand-context-lifts-to-module ctx)
       #t))

(define (syntax-local-transforming-module-provides?)
  (define ctx (current-expand-context))
  (and ctx
       (expand-context-requires+provides ctx)
       #t))
  
;; ----------------------------------------

(define (syntax-local-context)
  (define ctx (get-current-expand-context 'syntax-local-context))
  (expand-context-context ctx))

(define (syntax-local-introduce s)
  (check 'syntax-local-introduce syntax? s)
  (define ctx (get-current-expand-context 'syntax-local-introduce))
  (flip-introduction-scopes s ctx))

(define (syntax-local-identifier-as-binding id)
  (check syntax-local-identifier-as-binding identifier? id)
  (define ctx (get-current-expand-context 'syntax-local-identifier-as-binding))
  (remove-use-site-scopes id ctx))

(define (syntax-local-phase-level)
  (define ctx (current-expand-context))
  (if ctx
      (expand-context-phase ctx)
      0))

(define (syntax-local-name)
  (define ctx (get-current-expand-context 'syntax-local-name))
  (define id (expand-context-name ctx))
  (and id
       ;; Strip lexical context, but keep source-location information
       (datum->syntax #f (syntax-e id) id)))

;; ----------------------------------------

(define (make-syntax-introducer [as-use-site? #f])
  (define sc (new-scope (if as-use-site? 'use-site 'macro)))
  (lambda (s [mode 'flip])
    (check 'syntax-introducer syntax? s)
    (case mode
      [(add) (add-scope s sc)]
      [(remove) (remove-scope s sc)]
      [(flip) (flip-scope s sc)]
      [else (raise-argument-error 'syntax-introducer "(or/c 'add 'remove 'flip)" mode)])))

(define (make-syntax-delta-introducer ext-s base-s [phase (syntax-local-phase-level)])
  (check 'make-syntax-delta-introducer syntax? ext-s)
  (check 'make-syntax-delta-introducer syntax? base-s)
  (unless (phase? phase)
    (raise-argument-error 'make-syntax-delta-introducer phase?-string phase))
  (define ext-scs (syntax-scope-set ext-s phase))
  (define base-scs (syntax-scope-set base-s phase))
  (define delta-scs (set->list (set-subtract ext-scs base-scs)))
  (lambda (s [mode 'flip])
    (case mode
      [(add) (add-scopes s delta-scs)]
      [(remove) (remove-scopes s delta-scs)]
      [(flip) (flip-scopes s delta-scs)]
      [else (raise-argument-error 'syntax-introducer "(or/c 'add 'remove 'flip)" mode)])))
  
;; ----------------------------------------

(define (do-syntax-local-value who id [failure-thunk #f]
                               #:immediate? [immediate? #f])
  (check who identifier? id)
  (unless (or (not failure-thunk)
              (and (procedure? failure-thunk)
                   (procedure-arity-includes? failure-thunk 0)))
    (raise-argument-error who
                          "(or #f (procedure-arity-includes/c 0))" 
                          failure-thunk))
  (define ctx (get-current-expand-context who))
  (define phase (expand-context-phase ctx))
  (let loop ([id id])
    (define b (resolve+shift id phase #:immediate? immediate?))
    (cond
     [(not b)
      (if failure-thunk
          (failure-thunk)
          (error 'syntax-local-value "unbound identifier: ~v" id))]
     [else
      (define v (lookup b ctx id #:out-of-context-as-variable? #t))
      (cond
       [(or (variable? v) (core-form? v))
        (if failure-thunk
            (failure-thunk)
            (error 'syntax-local-value "identifier is not bound to syntax: ~v" id))]
       [(rename-transformer? v)
        (if immediate?
            (values v (rename-transformer-target v))
            (loop (rename-transformer-target v)))]
       [immediate? (values v #f)]
       [else v])])))

(define (syntax-local-value id [failure-thunk #f])
  (do-syntax-local-value 'syntax-local-value #:immediate? #f id failure-thunk))

(define (syntax-local-value/immediate id [failure-thunk #f])
  (do-syntax-local-value 'syntax-local-value/immediate #:immediate? #t id failure-thunk))

;; ----------------------------------------

(define (do-lift-values-expression who n s)
  (check who syntax? s)
  (check who exact-nonnegative-integer? n)
  (define ctx (get-current-expand-context who))
  (define lifts (expand-context-lifts ctx))
  (define ids (for/list ([i (in-range n)])
                ;; FIXME: use deterministic counter
                (define name (gensym 'lifted))
                (add-scope (datum->syntax #f name) (new-scope 'macro))))
  (map (lambda (id) (flip-introduction-scopes id ctx))
       ;; returns converted ids:
       (add-lifted! lifts
                    ids
                    (flip-introduction-scopes s ctx)
                    (expand-context-phase ctx))))

(define (syntax-local-lift-expression s)
  (car (do-lift-values-expression 'syntax-local-lift-expression 1 s)))

(define (syntax-local-lift-values-expression n s)
  (do-lift-values-expression 'syntax-local-lift-values-expression n s))

(define (syntax-local-lift-context)
  (define ctx (get-current-expand-context 'syntax-local-lift-context))
  (expand-context-lifts ctx))

;; ----------------------------------------

(define (syntax-local-lift-module s)
  (check 'syntax-local-lift-module syntax? s)
  (define ctx (get-current-expand-context 'syntax-local-lift-module))
  (define phase (expand-context-phase ctx))
  (case (core-form-sym s phase)
    [(module module*)
     (add-lifted-module! (expand-context-module-lifts ctx) s phase)]
    [else
     (raise-arguments-error 'syntax-local-lift-module "not a module form"
                            "given form" s)]))

;; ----------------------------------------

(define (do-local-lift-to-module who add! s filter
                                 #:intro? [intro? #t]
                                 #:more-checks [more-checks void])
  (check who syntax? s)
  (more-checks)
  (define ctx (get-current-expand-context who))
  (define phase (expand-context-phase ctx))
  (define lifts-to-module (expand-context-lifts-to-module ctx))
  (add! lifts-to-module
        (filter (if intro?
                    (flip-introduction-scopes s ctx)
                    s)
                phase
                (lift-to-module-context-end-as-expressions? lifts-to-module))
        phase))

(define (syntax-local-lift-require s use-s)
  (define sc (new-scope 'macro))
  (do-local-lift-to-module 'syntax-local-lift-module-require
                           add-lifted-to-module-require!
                           s #:intro? #f
                           #:more-checks
                           (lambda ()
                             (check 'syntax-local-lift-module-require
                                    syntax?
                                    use-s))
                           (lambda (s phase end-as-expressions?)
                             (wrap-form '#%require
                                        (add-scope s sc)
                                        phase)))
  (add-scope use-s sc))

(define (syntax-local-lift-provide s)
  (do-local-lift-to-module 'syntax-local-lift-module-end-declaration
                           add-lifted-to-module-provide!
                           s
                           (lambda (s phase end-as-expressions?)
                             (wrap-form '#%provide s phase))))

(define (syntax-local-lift-module-end-declaration s)
  (do-local-lift-to-module 'syntax-local-lift-module-end-declaration
                           add-lifted-to-module-end!
                           s
                           (lambda (orig-s phase end-as-expressions?)
                             (define s (if end-as-expressions?
                                           (wrap-form '#%expression orig-s phase)
                                           orig-s))
                             (for/fold ([s s]) ([phase (in-range phase 0 -1)])
                               (wrap-form 'begin-for-syntax
                                          s
                                          (sub1 phase))))))

(define (wrap-form sym s phase)
  (datum->syntax
   #f
   (list (datum->syntax
          (syntax-shift-phase-level core-stx phase)
          sym)
         s)))

;; ----------------------------------------

(define (syntax-local-module-defined-identifiers)
  (unless (syntax-local-transforming-module-provides?)
    (raise-arguments-error 'syntax-local-module-defined-identifiers "not currently transforming module provides"))
  (define ctx (current-expand-context))
  (requireds->phase-ht (extract-module-definitions (expand-context-requires+provides ctx))))
  
  
(define (syntax-local-module-required-identifiers mod-path phase-level)
  (unless (or (not mod-path) (module-path? mod-path))
    (raise-argument-error 'syntax-local-module-required-identifiers "(or/c module-path? #f)" mod-path))
  (unless (or (eq? phase-level #t) (phase? phase-level))
    (raise-argument-error 'syntax-local-module-required-identifiers (format "(or/c ~a #t)" phase?-string) phase-level))
  (unless (syntax-local-transforming-module-provides?)
    (raise-arguments-error 'syntax-local-module-required-identifiers "not currently transforming module provides"))
  (define ctx (current-expand-context))
  (define requires+provides (expand-context-requires+provides ctx))
  (define mpi (and mod-path
                   (module-path-index-join mod-path (requires+provides-self requires+provides))))
  (define requireds
    (extract-all-module-requires requires+provides
                                 mpi
                                 (if (eq? phase-level #t) 'all phase-level)))
  (for/list ([(phase ids) (in-hash (requireds->phase-ht requireds))])
    (cons phase ids)))

(define (requireds->phase-ht requireds)
  (for/fold ([ht (hasheqv)]) ([r (in-list requireds)])
    (hash-update ht
                 (required-phase r)
                 (lambda (l) (cons (required-id r) l))
                 null)))

;; ----------------------------------------

(define (syntax-local-module-exports mod-path)
  (check 'syntax-local-module-exports module-path? mod-path)
  (define ctx (get-current-expand-context 'syntax-local-module-exports))
  (define ns (expand-context-namespace ctx))
  (define mod-name (resolve-module-path mod-path (namespace-module-name ns)))
  (define m (namespace->module ns mod-name))
  (unless m (raise-unknown-module-error 'syntax-local-module-exports))
  (for/list ([(phase syms) (in-hash (module-provides m))])
    (cons phase
          (for/list ([sym (in-hash-keys syms)])
            sym))))

(define (syntax-local-submodules)
  (define ctx (get-current-expand-context 'syntax-local-submodules))
  (define submods (expand-context-declared-submodule-names ctx))
  (for/list ([(name kind) (in-hash (unbox submods))]
             #:when (eq? kind 'module))
    name))

;; ----------------------------------------

(define (syntax-local-get-shadower id [only-generated? #f])
  (check 'syntax-local-get-shadower identifier? id)
  (define ctx (get-current-expand-context 'syntax-local-get-shadower))
  (add-scopes id (set->list
                  (syntax-scope-set (expand-context-all-scopes-stx ctx)
                                    (expand-context-phase ctx)))))
