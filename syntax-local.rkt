#lang racket/base
(require racket/set
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "rename-trans.rkt"
         "lift-context.rkt")

(provide get-current-expand-context
         flip-introduction-scopes
         
         syntax-local-context
         syntax-local-introduce
         syntax-local-identifier-as-binding
         syntax-local-phase-level
         
         make-syntax-introducer
         make-syntax-delta-introducer
         
         syntax-local-value
         syntax-local-value/immediate
         
         syntax-local-lift-expression
         syntax-local-lift-values-expression
         syntax-local-lift-context)

;; ----------------------------------------

(define (get-current-expand-context who)
  (or (current-expand-context)
      (error who "not currently expanding")))

(define (flip-introduction-scopes s ctx)
  (flip-scopes s (expand-context-current-introduction-scopes ctx)))

;; ----------------------------------------

(define (syntax-local-context)
  (define ctx (get-current-expand-context 'syntax-local-context))
  (expand-context-context ctx))

(define (syntax-local-introduce s)
  (unless (syntax? s)
    (raise-argument-error 'syntax-local-introduce "syntax?" s))
  (define ctx (get-current-expand-context 'syntax-local-introduce))
  (flip-introduction-scopes s ctx))

(define (syntax-local-identifier-as-binding id)
  (unless (identifier? id)
    (raise-argument-error 'syntax-local-identifier-as-binding "identifier?" id))
  (define ctx (get-current-expand-context 'syntax-local-identifier-as-binding))
  (remove-use-site-scopes id ctx))

(define (syntax-local-phase-level)
  (define ctx (current-expand-context))
  (if ctx
      (expand-context-phase ctx)
      0))

;; ----------------------------------------

(define (make-syntax-introducer [as-use-site? #f])
  (define sc (new-scope))
  (lambda (s [mode 'flip])
    (unless (syntax? s)
      (raise-argument-error 'syntax-introducer "syntax?" s))
    (case mode
      [(add) (add-scope s sc)]
      [(remove) (remove-scope s sc)]
      [(flip) (flip-scope s sc)]
      [else (raise-argument-error 'syntax-introducer "(or/c 'add 'remove 'flip)" mode)])))

(define (make-syntax-delta-introducer ext-s base-s [phase (syntax-local-phase-level)])
  (unless (syntax? ext-s)
    (raise-argument-error 'make-syntax-delta-introducer "syntax?" ext-s))
  (unless (syntax? base-s)
    (raise-argument-error 'make-syntax-delta-introducer "syntax?" base-s))
  (unless (phase? phase)
    (raise-argument-error 'make-syntax-delta-introducer "phase?" phase))
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
  (unless (identifier? id)
    (raise-argument-error who "identifier?" id))
  (unless (or (not failure-thunk)
              (and (procedure? failure-thunk)
                   (procedure-arity-includes? failure-thunk 0)))
    (raise-argument-error who
                          "(or #f (procedure-arity-includes/c 0))" 
                          failure-thunk))
  (define ctx (get-current-expand-context 'syntax-local-value))
  (define phase (expand-context-phase ctx))
  (let loop ([id id])
    (define b (resolve id phase))
    (cond
     [(not b)
      (if failure-thunk
          (failure-thunk)
          (error 'syntax-local-value "unbound identifier: ~v" id))]
     [else
      (define v (lookup b ctx id))
      (cond
       [(or (variable? v) (unbound? v) (core-form? v))
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
  (unless (syntax? s)
    (raise-argument-error who "syntax?" s))
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error who "exact-nonnegative-integer?" n))
  (define ctx (get-current-expand-context who))
  (define lifts (expand-context-lifts ctx))
  (define ids (for/list ([i (in-range n)])
                ;; FIXME: use deterministic counter
                (define name (gensym 'lifted))
                (add-scope (datum->syntax #f name) (new-scope))))
  ;; returns converted ids:
  (add-lifted! lifts ids s (expand-context-phase ctx)))

(define (syntax-local-lift-expression s)
  (car (do-lift-values-expression 'syntax-local-lift-expression 1 s)))

(define (syntax-local-lift-values-expression n s)
  (do-lift-values-expression 'syntax-local-lift-values-expression n s))

(define (syntax-local-lift-context)
  (define ctx (get-current-expand-context 'syntax-local-lift-context))
  (expand-context-lifts ctx))
