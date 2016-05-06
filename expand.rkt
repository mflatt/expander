#lang racket/base
(require racket/set
         racket/unit
         "syntax.rkt"
         "scope.rkt"
         "match.rkt"
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
         
         remove-use-site-scopes
         rebuild)

;; ----------------------------------------

(define (expand s ctx)
  (cond
   [(identifier? s)
    (define binding (resolve s (expand-context-phase ctx)))
    (cond
     [(not binding)
      ;; The implicit `#%top` form handles unbound identifiers
      (expand-implicit '#%top s ctx)]
     [else
      ;; Variable or form as identifier macro
      (dispatch (lookup binding ctx s) s ctx)])]
   [(and (pair? (syntax-e s))
         (identifier? (car (syntax-e s))))
    ;; An "application" form that starts with an identifier
    (define id (car (syntax-e s)))
    (define binding (resolve id (expand-context-phase ctx)))
    (cond
     [(not binding)
      ;; The `#%app` binding might do something with unbound ids
      (expand-implicit '#%app s ctx)]
     [else
      ;; Find out whether it's bound as a variable, syntax, or core form
      (define t (lookup binding ctx id))
      (cond
       [(or (variable? t) (unbound? t))
        ;; Not as syntax or core form, so use implicit `#%app`
        (expand-implicit '#%app s ctx)]
       [else
        ;; Syntax or core form as "application"
        (dispatch t s ctx)])])]
   [(or (pair? (syntax-e s))
        (null? (syntax-e s)))
    ;; An "application" form that doesn't start with an identifier, so
    ;; use implicit `#%app`
    (expand-implicit '#%app s ctx)]
   [else
    ;; Anything other than an identifier or parens triggers the
    ;; implicit `#%datum` form
    (expand-implicit '#%datum s ctx)]))

;; Handle an implicit: `#%app`, `#%top`, or `#%datum`
(define (expand-implicit sym s ctx)
  (define id (datum->syntax s sym))
  ;; Instead of calling `expand` with a new form that starts `id`,
  ;; we reimplement the "applicaiton"-form case of `expand` so that
  ;; we provide an error if the implicit form is not suitably bound
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

;; Expand `s` given that the value `t` of the relevant binding,
;; where `t` is either a core form, a macro transformer, some
;; other compile-time value (which is an error), or a token
;; indincating that the binding is a run-time variable
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

;; Given a macro transformer `t`, apply it --- adding appropriate
;; scopes to represent the expansion step
(define (apply-transformer t s ctx)
  (define intro-scope (new-scope))
  (define intro-s (add-scope s intro-scope))
  ;; In a definition context, we need use-site scopes
  (define use-s (maybe-add-use-site-scope intro-s ctx))
  ;; Call the transformer; the current expansion context may be needed
  ;; for `syntax-local-....` functions
  (define transformed-s (parameterize ([current-expand-context ctx])
                          (t use-s)))
  (unless (syntax? transformed-s)
    (error "transformer produced non-syntax:" transformed-s))
  (define result-s (flip-scope transformed-s intro-scope))
  ;; In a definition contex, we need to add the inside-edge scope to
  ;; any expansion result
  (maybe-add-post-expansion-scope result-s ctx))

(define (maybe-add-use-site-scope s ctx)
  (cond
   [(expand-context-use-site-scopes ctx)
    ;; We're in a recursive definition context where use-site scopes
    ;; are needed, so create one, record it, and add to the given
    ;; syntax
    (define sc (new-scope))
    (define b (expand-context-use-site-scopes ctx))
    (set-box! b (cons sc (unbox b)))
    (add-scope s sc)]
   [else s]))

(define (maybe-add-post-expansion-scope s ctx)
  (cond
   [(expand-context-post-expansion-scope ctx)
    ;; We're in a definition context where an inside-edge scope needs
    ;; to be added to any immediate macro expansion; that way, if the
    ;; macro expands to a definition form, the binding will be in the
    ;; definition context's scope
    (add-scope s (expand-context-post-expansion-scope ctx))]
   [else s]))

;; Helper to lookup a binding in an expansion context
(define (lookup b ctx id)
  (binding-lookup b
                  (expand-context-env ctx)
                  (expand-context-namespace ctx)
                  (expand-context-phase ctx)
                  id))

;; ----------------------------------------

;; Expand a sequence of body forms in a definition context
(define (expand-body bodys sc s ctx)
  ;; The outside-edge scope identifies the original content of the
  ;; definition context
  (define outside-sc (new-scope))
  ;; The inside-edge scope identifiers any form that appears (perhaps
  ;; through macro expansion) in the definition context
  (define inside-sc (new-scope))
  (define init-bodys
    (for/list ([body (in-list bodys)])
      (add-scope (add-scope (add-scope body sc) outside-sc) inside-sc)))
  (define phase (expand-context-phase ctx))
  ;; Create an expansion context for expanding only immediate macros;
  ;; this partial-expansion phase uncovers macro- and variable
  ;; definitions in the definition context
  (define body-ctx (struct-copy expand-context ctx
                                [only-immediate? #t]
                                [post-expansion-scope inside-sc]
                                [scopes (list* outside-sc
                                               inside-sc
                                               (expand-context-scopes ctx))]
                                [use-site-scopes (box null)]))
  (let loop ([body-ctx body-ctx]
             [bodys init-bodys]
             [done-bodys null] ; accumulated expressions
             [val-binds null]  ; accumulated bindings
             [dups (make-check-no-duplicate-table)])
    (cond
     [(null? bodys)
      ;; Partial expansion is complete, so finish by rewriting to
      ;; `letrec-values`
      (finish-expanding-body body-ctx done-bodys val-binds s)]
     [else
      (define exp-body (expand (car bodys) body-ctx))
      (case (core-form-sym exp-body phase)
        [(begin)
         ;; Splice a `begin` form
         (define m (match-syntax exp-body '(begin e ...)))
         (loop body-ctx
               (append (m 'e) (cdr bodys))
               done-bodys
               val-binds
               dups)]
        [(define-values)
         ;; Found a variable definition; add bindings, extend the
         ;; environment, and continue
         (define m (match-syntax exp-body '(define-values (id ...) rhs)))
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
               (cons (list ids (m 'rhs))
                     ;; If we had accumulated some expressions, we
                     ;; need to turn each into a
                     ;;  (defined-values () (begin <expr> (values)))
                     ;; form so it can be kept with definitions to
                     ;; preserved order
                     (append
                      (for/list ([done-body (in-list done-bodys)])
                        (no-binds s phase))
                      val-binds))
               new-dups)]
        [(define-syntaxes)
         ;; Found a macro definition; add bindings, evaluate the
         ;; compile-time right-hand side, install the compile-time
         ;; values in the environment, and continue
         (define m (match-syntax exp-body '(define-syntaxes (id ...) rhs)))
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
               val-binds
               new-dups)]
        [else
         ;; Found an expression; accumulate it and continue
         (loop body-ctx
               (cdr bodys)
               (cons exp-body done-bodys)
               val-binds
               dups)])])))

;; Partial expansion is complete, so assumble the result as a
;; `letrec-values` form and continue expanding
(define (finish-expanding-body body-ctx done-bodys val-binds s)
  (when (null? done-bodys)
    (error "no body forms:" s))
  ;; To reference core forms at the current expansion phase:
  (define s-core-stx
    (syntax-shift-phase-level core-stx (expand-context-phase body-ctx)))
  ;; As we finish expanding, we're no longer in a definition context
  (define finish-ctx (struct-copy expand-context body-ctx
                                  [use-site-scopes #f]
                                  [scopes (append
                                           (unbox (expand-context-use-site-scopes body-ctx))
                                           (expand-context-scopes body-ctx))]
                                  [only-immediate? #f]
                                  [post-expansion-scope #f]))
  ;; Helper to expand and wrap the ending expressions in `begin`, if needed:
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
    ;; No definitions, so no `letrec-values` wrapper needed:
    (finish-bodys)]
   [else
    ;; Add `letrec-values` wrapper, finish expanding the right-hand
    ;; sides, and then finish the body expression:
    (datum->syntax
     #f
     `(,(datum->syntax s-core-stx 'letrec-values)
       ,(for/list ([bind (in-list (reverse val-binds))])
          `(,(datum->syntax #f (car bind)) ,(expand (cadr bind) finish-ctx)))
       ,(finish-bodys))
     s)]))

;; Helper to turn an expression into a binding clause with zero
;; bindings
(define (no-binds s phase)
  (define s-core-stx (syntax-shift-phase-level core-stx phase))
  (list null null (datum->syntax #f
                                 `(,(datum->syntax s-core-stx 'begin)
                                   (,(datum->syntax s-core-stx '#%app)
                                    ,(datum->syntax s-core-stx 'values)))
                                 s)))

;; Helper to remove any created use-site scopes from the left-hand
;; side of a definition that was revealed by partial expansion in a
;; definition context
(define (remove-use-site-scopes s ctx)
  (remove-scopes s (unbox (expand-context-use-site-scopes ctx))))

;; ----------------------------------------

;; Expand `s` as a compile-time expression relative to the current
;; expansion context
(define (expand-transformer s ctx)
  (expand s (struct-copy expand-context ctx
                         [scopes null]
                         [phase (add1 (expand-context-phase ctx))]
                         [env empty-env]
                         [only-immediate? #f]
                         [post-expansion-scope #f]
                         [module-scopes null])))

;; Expand and evaluate `s` as a compile-time expression, ensuring that
;; the number of returned values matches the number of target
;; identifiers; return the expanded form as well as its values
(define (expand+eval-for-syntaxes-binding rhs ids ctx)
  (define exp-rhs (expand-transformer rhs ctx))
  (values exp-rhs
          (eval-for-bindings ids
                             exp-rhs
                             (add1 (expand-context-phase ctx))
                             (expand-context-namespace ctx))))

;; Expand and evaluate `s` as a compile-time expression, returning
;; only the compile-time values
(define (eval-for-syntaxes-binding rhs ids ctx)
  (define-values (exp-rhs vals)
    (expand+eval-for-syntaxes-binding rhs ids ctx))
  vals)

;; Expand and evaluate `s` as an expression in the given phase;
;; ensuring that the number of returned values matches the number of
;; target identifiers; return the values
(define (eval-for-bindings ids s phase ns)
  (define compiled (compile s ns phase))
  (define vals
    (call-with-values (lambda () (expand-time-eval compiled))
      list))
  (unless (= (length vals) (length ids))
    (error "wrong number of results (" (length vals) "vs." (length ids) ")"
           "from" s))
  vals)

;; ----------------------------------------

;; A helper for forms to reconstruct syntax
(define (rebuild orig-s new)
  (datum->syntax orig-s new orig-s orig-s))
