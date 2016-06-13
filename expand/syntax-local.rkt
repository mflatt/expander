#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "env.rkt"
         "context.rkt"
         "expand.rkt"
         "../namespace/core.rkt"
         "use-site.rkt"
         "rename-trans.rkt"
         "lift-context.rkt"
         "require+provide.rkt"
         "../common/module-path.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../common/contract.rkt"
         "../syntax/debug.rkt")

(provide flip-introduction-scopes
         
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

(define (flip-introduction-scopes s ctx)
  (flip-scopes s (expand-context-current-introduction-scopes ctx)))

;; ----------------------------------------

(define (syntax-transforming?)
  (and (get-current-expand-context #:fail-ok? #t) #t))

(define (syntax-transforming-with-lifts?)
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (and ctx
       (expand-context-lifts ctx)
       #t))

(define (syntax-transforming-module-expression?)
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (and ctx
       (expand-context-to-module-lifts ctx)
       #t))

(define (syntax-local-transforming-module-provides?)
  (define ctx (get-current-expand-context #:fail-ok? #t))
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
  (define ctx (get-current-expand-context #:fail-ok? #t))
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
  (lambda (s [mode 'add])
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
  (define counter (root-expand-context-counter ctx))
  (define ids (for/list ([i (in-range n)])
                (set-box! counter (add1 (unbox counter)))
                (define name (string->unreadable-symbol (format "lifted/~a" (unbox counter))))
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

(define (do-local-lift-to-module who get add! s filter
                                 #:intro? [intro? #t]
                                 #:more-checks [more-checks void])
  (check who syntax? s)
  (more-checks)
  (define ctx (get-current-expand-context who))
  (define phase (expand-context-phase ctx))
  (define lift-ctx (get ctx))
  (add! lift-ctx
        (filter (if intro?
                    (flip-introduction-scopes s ctx)
                    s)
                phase
                lift-ctx)
        phase))

(define (syntax-local-lift-require s use-s)
  (define sc (new-scope 'macro))
  (do-local-lift-to-module 'syntax-local-lift-module-require
                           expand-context-require-lifts
                           add-lifted-require!
                           s #:intro? #f
                           #:more-checks
                           (lambda ()
                             (check 'syntax-local-lift-module-require
                                    syntax?
                                    use-s))
                           (lambda (s phase require-lift-ctx)
                             (wrap-form '#%require
                                        (add-scope s sc)
                                        phase)))
  (add-scope use-s sc))

(define (syntax-local-lift-provide s)
  (do-local-lift-to-module 'syntax-local-lift-module-end-declaration
                           expand-context-to-module-lifts
                           add-lifted-to-module-provide!
                           s
                           (lambda (s phase to-module-lift-ctx)
                             (wrap-form '#%provide s phase))))

(define (syntax-local-lift-module-end-declaration s)
  (do-local-lift-to-module 'syntax-local-lift-module-end-declaration
                           expand-context-to-module-lifts
                           add-lifted-to-module-end!
                           s
                           (lambda (orig-s phase to-module-lift-ctx)
                             (define s (if (to-module-lift-context-end-as-expressions? to-module-lift-ctx)
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
  (define ctx (get-current-expand-context 'syntax-local-module-defined-identifiers))
  (requireds->phase-ht (extract-module-definitions (expand-context-requires+provides ctx))))
  
  
(define (syntax-local-module-required-identifiers mod-path phase-level)
  (unless (or (not mod-path) (module-path? mod-path))
    (raise-argument-error 'syntax-local-module-required-identifiers "(or/c module-path? #f)" mod-path))
  (unless (or (eq? phase-level #t) (phase? phase-level))
    (raise-argument-error 'syntax-local-module-required-identifiers (format "(or/c ~a #t)" phase?-string) phase-level))
  (unless (syntax-local-transforming-module-provides?)
    (raise-arguments-error 'syntax-local-module-required-identifiers "not currently transforming module provides"))
  (define ctx (get-current-expand-context 'syntax-local-module-required-identifiers))
  (define requires+provides (expand-context-requires+provides ctx))
  (define mpi (and mod-path
                   (module-path-index-join mod-path (requires+provides-self requires+provides))))
  (define requireds
    (extract-all-module-requires requires+provides
                                 mpi
                                 (if (eq? phase-level #t) 'all phase-level)))
  (and requireds
       (for/list ([(phase ids) (in-hash (requireds->phase-ht requireds))])
         (cons phase ids))))

(define (requireds->phase-ht requireds)
  (for/fold ([ht (hasheqv)]) ([r (in-list requireds)])
    (hash-update ht
                 (required-phase r)
                 (lambda (l) (cons (required-id r) l))
                 null)))

;; ----------------------------------------

(define (syntax-local-module-exports mod-path)
  (unless (or (module-path? mod-path)
              (and (syntax? mod-path)
                   (module-path? (syntax->datum mod-path))))
    (raise-argument-error 'syntax-local-module-exports
                          (string-append
                           "(or/c module-path?\n"
                           "      (and/c syntax?\n"
                           "             (lambda (stx)\n"
                           "               (module-path? (syntax->datum stx)))))")
                          mod-path))
  (define ctx (get-current-expand-context 'syntax-local-module-exports))
  (define ns (expand-context-namespace ctx))
  (define mod-name (resolve-module-path (if (syntax? mod-path)
                                            (syntax->datum mod-path)
                                            mod-path)
                                        (module-path-index-resolve
                                         (namespace-mpi ns))))
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
                  (syntax-scope-set (root-expand-context-all-scopes-stx ctx)
                                    (expand-context-phase ctx)))))
