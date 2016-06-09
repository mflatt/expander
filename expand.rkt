#lang racket/base
(require "set.rkt"
         "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "namespace.rkt"
         "binding.rkt"
         "env.rkt"
         "syntax-error.rkt"
         "syntax-id-error.rkt"
         "free-id-set.rkt"
         "dup-check.rkt"
         "use-site.rkt"
         "compile.rkt"
         "eval-compiled-top.rkt"
         "core.rkt"
         "runtime-primitives.rkt"
         "expand-context.rkt"
         "lift-context.rkt"
         "already-expanded.rkt"
         "liberal-def-ctx.rkt"
         "rename-trans.rkt"
         "allowed-context.rkt"
         "debug.rkt"
         "reference-record.rkt")

(provide expand
         expand-body
         expand-and-split-bindings-by-reference
         lookup
         apply-transformer
         
         expand/capture-lifts
         expand-transformer
         expand+eval-for-syntaxes-binding
         eval-for-syntaxes-binding
         eval-for-bindings
         
         rebuild)

;; ----------------------------------------

(define (expand s ctx
                ;; Aplying a rename transformer substitutes
                ;; an id without changing `s`
                #:alternate-id [alternate-id #f])
  (cond
   [(identifier? s)
    (define id (or alternate-id s))
    (guard-stop
     id ctx s
     (define binding (resolve+shift id (expand-context-phase ctx)
                                    #:ambiguous-value 'ambiguous
                                    #:immediate? #t))
     (cond
      [(eq? binding 'ambiguous)
       (raise-ambigious-error id ctx)]
      [(not binding)
       ;; The implicit `#%top` form handles unbound identifiers
       (expand-implicit '#%top (substitute-alternate-id s alternate-id) ctx s)]
      [else
       ;; Variable or form as identifier macro
       (dispatch (lookup binding ctx s) s id ctx binding)]))]
   [(and (pair? (syntax-e s))
         (identifier? (car (syntax-e s))))
    ;; An "application" form that starts with an identifier
    (define id (or alternate-id (car (syntax-e s))))
    (guard-stop
     id ctx s
     (define binding (resolve+shift id (expand-context-phase ctx)
                                    #:ambiguous-value 'ambiguous
                                    #:immediate? #t))
     (cond
      [(eq? binding 'ambiguous)
       (raise-ambigious-error id ctx)]
      [(not binding)
       ;; The `#%app` binding might do something with unbound ids
       (expand-implicit '#%app (substitute-alternate-id s alternate-id) ctx id)]
      [else
       ;; Find out whether it's bound as a variable, syntax, or core form
       (define t (lookup binding ctx id))
       (cond
        [(variable? t)
         ;; Not as syntax or core form, so use implicit `#%app`
         (expand-implicit '#%app (substitute-alternate-id s alternate-id) ctx id)]
        [else
         ;; Syntax or core form as "application"
         (dispatch t s id ctx binding)])]))]
   [(or (pair? (syntax-e s))
        (null? (syntax-e s)))
    ;; An "application" form that doesn't start with an identifier, so
    ;; use implicit `#%app`
    (expand-implicit '#%app s ctx #f)]
   [(already-expanded? (syntax-e s))
    ;; An expression that is already fully expanded via `local-expand-expression`
    (define ae (syntax-e s))
    (unless (bound-identifier=? (root-expand-context-all-scopes-stx ctx)
                                (already-expanded-all-scopes-stx ae)
                                (expand-context-phase ctx))
      (raise-syntax-error #f
                          (string-append "expanded syntax not in its original lexical context;\n"
                                         " extra bindings or scopes in the current context")
                          (already-expanded-s ae)))
    (already-expanded-s ae)]
   [else
    ;; Anything other than an identifier or parens triggers the
    ;; implicit `#%datum` form
    (expand-implicit '#%datum s ctx #f)]))

;; Handle an implicit: `#%app`, `#%top`, or `#%datum`
(define (expand-implicit sym s ctx trigger-id)
  (define id (datum->syntax s sym))
  (guard-stop
   id ctx s
   ;; Instead of calling `expand` with a new form that starts `id`,
   ;; we reimplement the "applicaiton"-form case of `expand` so that
   ;; we provide an error if the implicit form is not suitably bound
   (define b (resolve+shift id (expand-context-phase ctx)
                            #:ambiguous-value 'ambiguous
                            #:immediate? #t))
   (when (eq? b 'ambiguous)
     (raise-ambigious-error id ctx))
   (define t (and b (lookup b ctx id)))
   (cond
    [(core-form? t)
     (if (expand-context-only-immediate? ctx)
         s
         (dispatch t (datum->syntax s (cons sym s) s) id ctx b))]
    [(transformer? t)
     (dispatch t (datum->syntax s (cons sym s) s) id ctx b)]
    [else
     (define phase (expand-context-phase ctx))
     (define what
       (case sym
         [(#%app) "function application"]
         [(#%datum) "literal data"]
         [(#%top)
          (if (expand-context-allow-unbound? ctx)
              "reference to a top-level identifier"
              "reference to an unbound identifier")]))
     (define unbound? (and trigger-id (not (resolve trigger-id phase))))
     (raise-syntax-error #f
                         (format (if unbound?
                                     "unbound identifier;\n also, no ~a transformer is bound~a"
                                     (string-append what " is not allowed;\n no ~a syntax transformer is bound~a"))
                                 sym
                                 (case phase
                                   [(0) ""]
                                   [(1) " in the transformer phase"]
                                   [else (format " at phase ~a" phase)]))
                         (and unbound? trigger-id) (and (not unbound?) s) null
                         (if unbound? (syntax-debug-info-string trigger-id ctx) ""))])))

;; Expand `s` given that the value `t` of the relevant binding,
;; where `t` is either a core form, a macro transformer, some
;; other compile-time value (which is an error), or a token
;; indincating that the binding is a run-time variable
(define (dispatch t s id ctx binding)
  (cond
   [(core-form? t)
    (if (expand-context-only-immediate? ctx)
        s
        ((core-form-expander t) s ctx))]
   [(transformer? t)
    (cond
     [(not-in-this-expand-context? t ctx)
      (expand (avoid-current-expand-context (substitute-alternate-id s id) t ctx)
              ctx)]
     [else
      ;; Apply transformer and expand again
      (define-values (exp-s re-ctx)
        (apply-transformer (transformer->procedure t) s id ctx binding))
      (expand exp-s re-ctx
              #:alternate-id (and (rename-transformer? t)
                                  (rename-transformer-target t)))])]
   [(variable? t)
    ;; A reference to a variable expands to itself --- but if the
    ;; binding's frame has a reference record, then register the
    ;; use
    (when (and (not (expand-context-only-immediate? ctx))
               (local-binding? binding)
               (reference-record? (binding-frame-id binding)))
      (reference-record-used! (binding-frame-id binding) (local-binding-key binding)))
    ;; If the variable is locally bound, replace the use's scopes with the binding's scopes
    (substitute-variable id t)]
   [else
    ;; Some other compile-time value:
    (raise-syntax-error #f "illegal use of syntax" t)]))

;; Given a macro transformer `t`, apply it --- adding appropriate
;; scopes to represent the expansion step
(define (apply-transformer t s id ctx binding)
  (define intro-scope (new-scope 'macro))
  (define intro-s (add-scope s intro-scope))
  ;; In a definition context, we need use-site scopes
  (define-values (use-s use-scopes) (maybe-add-use-site-scope intro-s ctx binding))
  ;; Call the transformer; the current expansion context may be needed
  ;; for `syntax-local-....` functions, and we may accumulate scopes from
  ;; definition contexts created by the transformer
  (define def-ctx-scopes (box null))
  (define m-ctx (struct-copy expand-context ctx
                             [current-introduction-scopes (cons intro-scope
                                                                use-scopes)]
                             [def-ctx-scopes def-ctx-scopes]))
  (define transformed-s (parameterize ([current-expand-context m-ctx]
                                       [current-namespace (namespace->namespace-at-phase
                                                           (expand-context-namespace ctx)
                                                           (add1 (expand-context-phase ctx)))])
                          (t use-s)))
  (unless (syntax? transformed-s)
    (raise-argument-error (syntax-e id)
                          "received value from syntax expander was not syntax"
                          "received" transformed-s))
  (define result-s (flip-scope transformed-s intro-scope))
  (values
   ;; In a definition context, we need to add the inside-edge scope to
   ;; any expansion result
   (maybe-add-post-expansion-scope result-s ctx)
   ;; Move any accumulated definition-context scopes to the `scopes`
   ;; list for further expansion:
   (if (null? (unbox def-ctx-scopes))
       ctx
       (struct-copy expand-context ctx
                    [scopes (append (unbox def-ctx-scopes)
                                    (expand-context-scopes ctx))]))))

(define (maybe-add-use-site-scope s ctx binding)
  (cond
   [(and (root-expand-context-use-site-scopes ctx)
         (root-expand-context-frame-id ctx)
         (eq? (root-expand-context-frame-id ctx)
              (binding-frame-id binding)))
    ;; We're in a recursive definition context where use-site scopes
    ;; are needed, so create one, record it, and add to the given
    ;; syntax
    (define sc (new-scope 'use-site))
    (define b (root-expand-context-use-site-scopes ctx))
    (set-box! b (cons sc (unbox b)))
    (values (add-scope s sc) (list sc))]
   [else (values s null)]))

(define (maybe-add-post-expansion-scope s ctx)
  (cond
   [(root-expand-context-post-expansion-scope ctx)
    ;; We're in a definition context where an inside-edge scope needs
    ;; to be added to any immediate macro expansion; that way, if the
    ;; macro expands to a definition form, the binding will be in the
    ;; definition context's scope
    (case (expand-context-post-expansion-scope-mode ctx)
      [(add)
       (add-scope s (root-expand-context-post-expansion-scope ctx))]
      [(push)
       (push-scope s (root-expand-context-post-expansion-scope ctx))])]
   [else s]))

;; Helper to lookup a binding in an expansion context
(define (lookup b ctx id
                #:out-of-context-as-variable? [out-of-context-as-variable? #f])
  (binding-lookup b
                  (expand-context-env ctx)
                  (expand-context-lift-envs ctx)
                  (expand-context-namespace ctx)
                  (expand-context-phase ctx)
                  id
                  #:out-of-context-as-variable? out-of-context-as-variable?))

(define-syntax-rule (guard-stop id ctx s otherwise ...)
  (cond
   [(free-id-set-member? (expand-context-stops ctx)
                         (expand-context-phase ctx)
                         id)
    s]
   [else
    otherwise ...]))


(define (substitute-alternate-id s alternate-id)
  (cond
   [(not alternate-id) s]
   [(identifier? s) alternate-id]
   (datum->syntax
    s
    (cons alternate-id
          (cdr (syntax-e s)))
    s
    s)))

;; ----------------------------------------

;; Expand a sequence of body forms in a definition context
(define (expand-body bodys sc s ctx #:stratified? [stratified? #f])
  ;; The outside-edge scope identifies the original content of the
  ;; definition context
  (define outside-sc (new-scope 'local))
  ;; The inside-edge scope identifiers any form that appears (perhaps
  ;; through macro expansion) in the definition context
  (define inside-sc (new-scope 'intdef))
  (define init-bodys
    (for/list ([body (in-list bodys)])
      (add-scope (add-scope (if sc (add-scope body sc) body) outside-sc) inside-sc)))
  (define phase (expand-context-phase ctx))
  (define frame-id (make-reference-record)) ; accumulates info on referenced variables
  ;; Create an expansion context for expanding only immediate macros;
  ;; this partial-expansion phase uncovers macro- and variable
  ;; definitions in the definition context
  (define body-ctx (struct-copy expand-context ctx
                                [context (list (make-liberal-define-context))]
                                [only-immediate? #t]
                                [post-expansion-scope #:parent root-expand-context inside-sc]
                                [post-expansion-scope-mode 'add]
                                [scopes (list* outside-sc
                                               inside-sc
                                               (expand-context-scopes ctx))]
                                [use-site-scopes #:parent root-expand-context (box null)]
                                [frame-id #:parent root-expand-context frame-id]
                                [all-scopes-stx
                                 #:parent root-expand-context
                                 (add-scope
                                  (add-scope (root-expand-context-all-scopes-stx ctx)
                                             outside-sc)
                                  inside-sc)]))
  (let loop ([body-ctx body-ctx]
             [bodys init-bodys]
             [done-bodys null] ; accumulated expressions
             [val-idss null]   ; accumulated binding identifiers
             [val-keyss null]  ; accumulated binding keys
             [val-rhss null]   ; accumulated binding right-hand sides
             [dups (make-check-no-duplicate-table)])
    (cond
     [(null? bodys)
      ;; Partial expansion is complete, so finish by rewriting to
      ;; `letrec-values`
      (finish-expanding-body body-ctx frame-id
                             (reverse val-idss) (reverse val-keyss) (reverse val-rhss)
                             (reverse done-bodys)
                             s
                             #:stratified? stratified?)]
     [else
      (define exp-body (expand (car bodys) body-ctx))
      (case (core-form-sym exp-body phase)
        [(begin)
         ;; Splice a `begin` form
         (define m (match-syntax exp-body '(begin e ...)))
         (loop body-ctx
               (append (m 'e) (cdr bodys))
               done-bodys
               val-idss
               val-keyss
               val-rhss
               dups)]
        [(define-values)
         ;; Found a variable definition; add bindings, extend the
         ;; environment, and continue
         (define m (match-syntax exp-body '(define-values (id ...) rhs)))
         (define ids (remove-use-site-scopes (m 'id) body-ctx))
         (define new-dups (check-no-duplicate-ids ids phase exp-body dups))
         (define counter (root-expand-context-counter ctx))
         (define keys (for/list ([id (in-list ids)])
                        (add-local-binding! id phase counter #:frame-id frame-id)))
         (define extended-env (for/fold ([env (expand-context-env body-ctx)]) ([key (in-list keys)]
                                                                               [id (in-list ids)])
                                (env-extend env key (local-variable id))))
         (loop (struct-copy expand-context body-ctx
                            [env extended-env])
               (cdr bodys)
               null
               ;; If we had accumulated some expressions, we
               ;; need to turn each into the equivalent of
               ;;  (defined-values () (begin <expr> (values)))
               ;; form so it can be kept with definitions to
               ;; preserved order
               (cons ids (append
                          (for/list ([done-body (in-list done-bodys)])
                            null)
                          val-idss))
               (cons keys (append
                           (for/list ([done-body (in-list done-bodys)])
                             null)
                           val-keyss))
               (cons (m 'rhs) (append
                               (for/list ([done-body (in-list done-bodys)])
                                 (no-binds done-body s phase))
                               val-rhss))
               new-dups)]
        [(define-syntaxes)
         ;; Found a macro definition; add bindings, evaluate the
         ;; compile-time right-hand side, install the compile-time
         ;; values in the environment, and continue
         (define m (match-syntax exp-body '(define-syntaxes (id ...) rhs)))
         (define ids (remove-use-site-scopes (m 'id) body-ctx))
         (define new-dups (check-no-duplicate-ids ids phase exp-body dups))
         (define counter (root-expand-context-counter ctx))
         (define keys (for/list ([id (in-list ids)])
                        (add-local-binding! id phase counter #:frame-id frame-id)))
         (define vals (eval-for-syntaxes-binding (m 'rhs) ids ctx))
         (define extended-env (for/fold ([env (expand-context-env body-ctx)]) ([key (in-list keys)]
                                                                               [val (in-list vals)]
                                                                               [id (in-list ids)])
                                (maybe-install-free=id! val id phase)
                                (env-extend env key val)))
         (loop (struct-copy expand-context body-ctx
                            [env extended-env])
               (cdr bodys)
               done-bodys
               val-idss
               val-keyss
               val-rhss
               new-dups)]
        [else
         (cond
          [stratified?
           ;; Found an expression, so no more definitions are allowed
           (loop body-ctx
                 null
                 (append (reverse bodys) (cons exp-body done-bodys))
                 val-idss
                 val-keyss
                 val-rhss
                 dups)]
          [else
           ;; Found an expression; accumulate it and continue
           (loop body-ctx
                 (cdr bodys)
                 (cons exp-body done-bodys)
                 val-idss
                 val-keyss
                 val-rhss
                 dups)])])])))

;; Partial expansion is complete, so assumble the result as a
;; `letrec-values` form and continue expanding
(define (finish-expanding-body body-ctx frame-id
                               val-idss val-keyss val-rhss
                               done-bodys
                               s
                               #:stratified? stratified?)
  (when (null? done-bodys)
    (raise-syntax-error #f "no expression after a sequence of internal definitions" s))
  ;; To reference core forms at the current expansion phase:
  (define s-core-stx
    (syntax-shift-phase-level core-stx (expand-context-phase body-ctx)))
  ;; As we finish expanding, we're no longer in a definition context
  (define finish-ctx (struct-copy expand-context body-ctx
                                  [context 'expression]
                                  [use-site-scopes #:parent root-expand-context #f]
                                  [scopes (append
                                           (unbox (root-expand-context-use-site-scopes body-ctx))
                                           (expand-context-scopes body-ctx))]
                                  [only-immediate? #f]
                                  [post-expansion-scope #:parent root-expand-context #f]))
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
   [(null? val-idss)
    ;; No definitions, so no `letrec-values` wrapper needed:
    (finish-bodys)]
   [else
    ;; Roughly, finish expanding the right-hand sides, finish the body
    ;; expression, then add a `letrec-values` wrapper:
    (expand-and-split-bindings-by-reference
     val-idss val-keyss val-rhss
     #:split? (not stratified?)
     #:frame-id frame-id #:ctx finish-ctx #:source s
     #:get-body finish-bodys)]))

;; Roughly, create a `letrec-values` for for the given ids, right-hand sides, and
;; body. While expanding right-hand sides, though, keep track of whether any
;; forward references appear, and if not, generate a `let-values` form, instead,
;; at each binding clause. Similar, end a `letrec-values` form and start a new
;; one if there were forward references up to the clause but not beyond.
(define (expand-and-split-bindings-by-reference idss keyss rhss
                                                #:split? split?
                                                #:frame-id frame-id #:ctx ctx #:source s
                                                #:get-body get-body)
  (define s-core-stx
    (syntax-shift-phase-level core-stx (expand-context-phase ctx)))
  (let loop ([idss idss] [keyss keyss] [rhss rhss] [accum-idss null] [accum-rhss null])
    (cond
     [(null? idss)
      (cond
       [(null? accum-idss) (get-body)]
       [else
        (datum->syntax
         #f
         `(,(datum->syntax s-core-stx 'letrec-values)
           ,(map list (reverse accum-idss) (reverse accum-rhss))
           ,(loop (cdr idss) (cdr keyss) (cdr rhss) null null))
         s)])]
     [else
      (define ids (car idss))
      (define expanded-rhs (expand (car rhss) (as-named-context ctx ids)))
      
      (define local-or-forward-references? (reference-record-forward-references? frame-id))
      (reference-record-bound! frame-id (car keyss))
      (define forward-references? (reference-record-forward-references? frame-id))
      
      (cond
       [(and (not local-or-forward-references?)
             split?)
        (unless (null? accum-idss) (error "internal error: accumulated ids not empty"))
        (datum->syntax
         #f
         `(,(datum->syntax s-core-stx 'let-values)
           ([,ids ,expanded-rhs])
           ,(loop (cdr idss) (cdr keyss) (cdr rhss) null null))
         s)]
       [(and (not forward-references?)
             (or split? (null? (cdr idss))))
        (datum->syntax
         #f
         `(,(datum->syntax s-core-stx 'letrec-values)
           ,(map list
                 (reverse (cons ids accum-idss))
                 (reverse (cons expanded-rhs accum-rhss)))
           ,(loop (cdr idss) (cdr keyss) (cdr rhss) null null))
         s)]
       [else
        (loop (cdr idss) (cdr keyss) (cdr rhss)
              (cons ids accum-idss)
              (cons expanded-rhs accum-rhss))])])))

;; Helper to turn an expression into a binding clause with zero
;; bindings
(define (no-binds expr s phase)
  (define s-core-stx (syntax-shift-phase-level core-stx phase))
  (define s-runtime-stx (syntax-shift-phase-level runtime-stx phase))
  (datum->syntax #f
                 `(,(datum->syntax s-core-stx 'begin)
                   ,expr
                   (,(datum->syntax s-core-stx '#%app)
                    ,(datum->syntax s-runtime-stx 'values)))
                 s))

;; ----------------------------------------

;; Expand `s` as a compile-time expression relative to the current
;; expansion context
(define (expand/capture-lifts s ctx
                              #:expand-lifts? [expand-lifts? #f]
                              #:begin-form? [begin-form? #f])
  (define context (expand-context-context ctx))
  (define phase (expand-context-phase ctx))
  (define local? (not (memq context '(top-level module))))
  ;; Expand `s`, but loop to handle lifted expressions
  (let loop ([s s])
    (define lift-env (and local? (box empty-env)))
    (define capture-ctx (struct-copy expand-context ctx
                                     [lifts (make-lift-context
                                             (if local?
                                                 (make-local-lift lift-env (root-expand-context-counter ctx))
                                                 (make-toplevel-lift ctx)))]
                                     [lift-envs (if local?
                                                    (cons lift-env
                                                          (expand-context-lift-envs ctx))
                                                    (expand-context-lift-envs ctx))]))
    (define exp-s (expand s capture-ctx))
    (define lifts (get-and-clear-lifts! (expand-context-lifts capture-ctx)))
    (cond
     [(null? lifts)
      ;; No lifts, so expansion is done
      exp-s]
     [else
      ;; Have lifts, so wrap as `let-values` and expand again
      (define with-lifts-s
        (if begin-form?
            (wrap-lifts-as-begin lifts exp-s s phase)
            (wrap-lifts-as-let lifts exp-s s phase)))
      (if expand-lifts?
          (loop with-lifts-s)
          with-lifts-s)])))

;; Expand `s` as a compile-time expression relative to the current
;; expansion context
(define (expand-transformer s ctx
                            #:context [context 'expression]
                            #:begin-form? [begin-form? #f])
  (define phase (add1 (expand-context-phase ctx)))
  (define trans-ctx (struct-copy expand-context ctx
                                 [context context]
                                 [scopes null]
                                 [phase phase]
                                 [namespace (namespace->namespace-at-phase
                                             (expand-context-namespace ctx)
                                             phase)]
                                 [env empty-env]
                                 [only-immediate? #f]
                                 [post-expansion-scope #:parent root-expand-context #f]))
  (expand/capture-lifts s trans-ctx #:expand-lifts? #t #:begin-form? begin-form?))

;; Expand and evaluate `s` as a compile-time expression, ensuring that
;; the number of returned values matches the number of target
;; identifiers; return the expanded form as well as its values
(define (expand+eval-for-syntaxes-binding rhs ids ctx)
  (define exp-rhs (expand-transformer rhs (as-named-context ctx ids)))
  (define phase (add1 (expand-context-phase ctx)))
  (values exp-rhs
          (eval-for-bindings ids
                             exp-rhs
                             phase
                             (namespace->namespace-at-phase
                              (expand-context-namespace ctx)
                              phase)
                             ctx)))

;; Expand and evaluate `s` as a compile-time expression, returning
;; only the compile-time values
(define (eval-for-syntaxes-binding rhs ids ctx)
  (define-values (exp-rhs vals)
    (expand+eval-for-syntaxes-binding rhs ids ctx))
  vals)

;; Expand and evaluate `s` as an expression in the given phase;
;; ensuring that the number of returned values matches the number of
;; target identifiers; return the values
(define (eval-for-bindings ids s phase ns ctx)
  (define compiled (compile-single s (make-compile-context
                                      #:namespace ns
                                      #:phase phase)))
  (define vals
    (call-with-values (lambda ()
                        (parameterize ([current-expand-context ctx])
                          (eval-top compiled ns)))
      list))
  (unless (= (length vals) (length ids))
    (error "wrong number of results (" (length vals) "vs." (length ids) ")"
           "from" s))
  vals)

;; ----------------------------------------

;; A helper for forms to reconstruct syntax
(define (rebuild orig-s new)
  (datum->syntax orig-s new orig-s orig-s))
