#lang racket/base
(require racket/set
         racket/unit
         "syntax.rkt"
         "scope.rkt"
         "pattern.rkt"
         "namespace.rkt"
         "binding.rkt"
         "dup-check.rkt"
         "compile.rkt"
         "require.rkt"
         "core.rkt"
         "expand-context.rkt")

(provide expand
         expand-body
         lookup
         
         expand+eval-for-syntaxes-binding
         eval-for-syntaxes-binding
         eval-for-bindings
         
         rebuild)

;; ----------------------------------------

(define (expand s [ctx (current-expand-context)])
  (cond
   [(identifier? s)
    (define binding (resolve s (expand-context-phase ctx)))
    (cond
     [(not binding)
      (expand-implicit '#%top s ctx)]
     [else
      (dispatch (lookup binding ctx s) s ctx)])]
   [(and (pair? (syntax-e s))
         (identifier? (car (syntax-e s))))
    (define id (car (syntax-e s)))
    (define binding (resolve id (expand-context-phase ctx)))
    (cond
     [(not binding) (expand-implicit '#%app s ctx)]
     [else
      (define t (lookup binding ctx id))
      (cond
       [(or (variable? t) (unbound? t))
        (expand-implicit '#%app s ctx)]
       [else (dispatch t s ctx)])])]
   [(pair? (syntax-e s))
    (expand-implicit '#%app s ctx)]
   [(null? (syntax-e s))
    (error "empty application:" s)]
   [else
    (expand-implicit '#%datum s ctx)]))

(define (expand-implicit sym s ctx)
  (define id (datum->syntax s sym))
  (define b (resolve id (expand-context-phase ctx)))
  (define t (and b (lookup b ctx id)))
  (cond
   [(core-form? t)
    (if (expand-context-only-immediate? ctx)
        s
        (dispatch t (datum->syntax s (cons sym s) s) ctx))]
   [(transformer? t)
    (dispatch t (datum->syntax s (cons sym s) s) ctx)]
   [else
    (error (format "no transformer binding for ~a:" sym)
           s)]))

(define (dispatch t s ctx)
  (cond
   [(core-form? t)
    (if (expand-context-only-immediate? ctx)
        s
        ((core-form-expander t) s ctx))]
   [(transformer? t)
    ;; Apply transformer and expand again
    (expand (apply-transformer t s ctx) ctx)]
   [(or (variable? t)
        (unbound? t)) ;; treat unbound as variable (for top level)
    ;; A reference to a variable expands to itself
    s]
   [else
    ;; Some other compile-time value:
    (error "illegal use of syntax:" t)]))

(define (apply-transformer t s ctx)
  (define intro-scope (new-scope))
  (define input-s (add-scope (maybe-add-use-site-scope s ctx)
                             intro-scope))
  (define output-s (parameterize ([current-expand-context ctx])
                     (t input-s)))
  (unless (syntax? output-s)
    (error "transformer produced non-syntax:" output-s))
  (define exp-s (flip-scope output-s intro-scope))
  (if (expand-context-add-scope ctx)
      (add-scope exp-s (expand-context-add-scope ctx))
      exp-s))

(define (maybe-add-use-site-scope s ctx)
  (cond
   [(expand-context-use-site-scopes ctx)
    ;; We're in a recursive definition context where
    ;; use-site scopes are needed. Create one, record 
    ;; it, and add to the given syntax.
    (define sc (new-scope))
    (define b (expand-context-use-site-scopes ctx))
    (set-box! b (cons sc (unbox b)))
    (add-scope s sc)]
   [else s]))

(define (lookup b ctx id)
  (binding-lookup b
                  (expand-context-env ctx)
                  (expand-context-namespace ctx)
                  (expand-context-phase ctx)
                  id))

;; ----------------------------------------

(define (expand-body bodys sc s ctx)
  (define outside-sc (new-scope))
  (define inside-sc (new-scope))
  (define phase (expand-context-phase ctx))
  (define body-ctx (struct-copy expand-context ctx
                                [only-immediate? #t]
                                [add-scope inside-sc]
                                [scopes (list* outside-sc
                                               inside-sc
                                               (expand-context-scopes ctx))]
                                [use-site-scopes (box null)]))
  (let loop ([body-ctx body-ctx]
             [bodys (for/list ([body (in-list bodys)])
                      (add-scope (add-scope (add-scope body sc) outside-sc) inside-sc))]
             [done-bodys null]
             [trans-binds null] ; for `expand-once`, eventually
             [val-binds null]
             [dups (make-check-no-duplicate-table)])
    (cond
     [(null? bodys)
      (finish-expanding-body body-ctx done-bodys val-binds s)]
     [else
      (define exp-body (expand (car bodys) body-ctx))
      (case (core-form-sym exp-body phase)
        [(begin)
         (define m (parse-syntax exp-body '(begin e ...)))
         (loop body-ctx
               (append (m 'e) (cdr bodys))
               done-bodys
               trans-binds
               val-binds
               dups)]
        [(define-values)
         (define m (parse-syntax exp-body '(define-values (id ...) rhs)))
         (define ids (remove-use-site-scopes (m 'id) body-ctx))
         (define new-dups (check-no-duplicate-ids ids phase exp-body dups))
         (define keys (for/list ([id (in-list ids)])
                        (add-local-binding! id phase)))
         (define extended-env (for/fold ([env (expand-context-env body-ctx)]) ([key (in-list keys)])
                                (env-extend env key variable)))
         (loop (struct-copy expand-context body-ctx
                            [env extended-env])
               (cdr bodys)
               null
               trans-binds
               (cons (list ids (m 'rhs))
                     (append
                      (for/list ([done-body (in-list done-bodys)])
                        (no-binds s phase))
                      val-binds))
               new-dups)]
        [(define-syntaxes)
         (define m (parse-syntax exp-body '(define-syntaxes (id ...) rhs)))
         (define ids (remove-use-site-scopes (m 'id) body-ctx))
         (define new-dups (check-no-duplicate-ids ids phase exp-body dups))
         (define keys (for/list ([id (in-list ids)])
                        (add-local-binding! id phase)))
         (define vals (eval-for-syntaxes-binding (m 'rhs) ids ctx))
         (define extended-env (for/fold ([env (expand-context-env body-ctx)]) ([key (in-list keys)]
                                                                               [val (in-list vals)])
                                (env-extend env key val)))
         (loop (struct-copy expand-context body-ctx
                            [env extended-env])
               (cdr bodys)
               done-bodys
               (cons (list ids (m 'rhs))
                     trans-binds)
               val-binds
               new-dups)]
        [else
         (loop body-ctx
               (cdr bodys)
               (cons exp-body done-bodys)
               trans-binds
               val-binds
               dups)])])))

(define (no-binds s phase)
  (define s-core-stx (syntax-shift-phase-level core-stx phase))
  (list null null (datum->syntax #f
                                 `(,(datum->syntax s-core-stx 'begin)
                                   (,(datum->syntax s-core-stx '#%app)
                                    ,(datum->syntax s-core-stx 'values)))
                                 s)))

(define (finish-expanding-body body-ctx done-bodys val-binds s)
  (when (null? done-bodys)
    (error "no body forms:" s))
  (define s-core-stx
    (syntax-shift-phase-level core-stx (expand-context-phase body-ctx)))
  (define finish-ctx (struct-copy expand-context body-ctx
                                  [use-site-scopes #f]
                                  [scopes (append
                                           (unbox (expand-context-use-site-scopes body-ctx))
                                           (expand-context-scopes body-ctx))]
                                  [only-immediate? #f]
                                  [add-scope #f]))
  (define (finish-bodys)
    (cond
     [(null? (cdr done-bodys))
      (expand (car done-bodys) finish-ctx)]
     [else
      (datum->syntax
       #f
       `(,(datum->syntax s-core-stx 'begin)
         ,@(for/list ([body (in-list done-bodys)])
             (expand body finish-ctx)))
       s)]))
  (cond
   [(null? val-binds)
    (finish-bodys)]
   [else
    (datum->syntax
     #f
     `(,(datum->syntax s-core-stx 'letrec-values)
       ,(for/list ([bind (in-list (reverse val-binds))])
          `(,(datum->syntax #f (car bind)) ,(expand (cadr bind) finish-ctx)))
       ,(finish-bodys))
     s)]))

(define (remove-use-site-scopes s ctx)
  (remove-scopes s (unbox (expand-context-use-site-scopes ctx))))

;; ----------------------------------------

(define (expand-transformer s ctx)
  (expand s (struct-copy expand-context ctx
                         [scopes null]
                         [phase (add1 (expand-context-phase ctx))]
                         [env empty-env]
                         [only-immediate? #f]
                         [add-scope #f]
                         [module-scopes null])))

(define (expand+eval-for-syntaxes-binding rhs ids ctx)
  (define exp-rhs (expand-transformer rhs ctx))
  (values exp-rhs
          (eval-for-bindings ids
                             exp-rhs
                             (add1 (expand-context-phase ctx))
                             (expand-context-namespace ctx))))

(define (eval-for-syntaxes-binding rhs ids ctx)
  (define-values (exp-rhs vals)
    (expand+eval-for-syntaxes-binding rhs ids ctx))
  vals)

(define (eval-for-bindings ids s phase ns)
  (define compiled (compile s phase ns))
  (define vals
    (call-with-values (lambda () (expand-time-eval `(#%expression ,compiled)))
      list))
  (unless (= (length vals) (length ids))
    (error "wrong number of results (" (length vals) "vs." (length ids) ")"
           "from" s))
  vals)

;; ----------------------------------------

;; A helper for forms to reconstruct syntax
(define (rebuild orig-s new)
  (datum->syntax orig-s new orig-s orig-s))
