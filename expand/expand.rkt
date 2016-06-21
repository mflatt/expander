#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/inspector.rkt"
         "../syntax/binding.rkt"
         "env.rkt"
         "../syntax/track.rkt"
         "../syntax/error.rkt"
         "syntax-id-error.rkt"
         "free-id-set.rkt"
         "dup-check.rkt"
         "use-site.rkt"
         "../compile/main.rkt"
         "../eval/top.rkt"
         "../namespace/core.rkt"
         "../boot/runtime-primitive.rkt"
         "context.rkt"
         "lift-context.rkt"
         "already-expanded.rkt"
         "liberal-def-ctx.rkt"
         "rename-trans.rkt"
         "allowed-context.rkt"
         "lift-key.rkt"
         "../syntax/debug.rkt"
         "reference-record.rkt"
         "log.rkt")

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
         
         rebuild
         attach-disappeared-transformer-bindings)

;; ----------------------------------------

(define (expand s ctx
                ;; Aplying a rename transformer substitutes
                ;; an id without changing `s`
                #:alternate-id [alternate-id #f])
  (log-expand ctx (if (expand-context-only-immediate? ctx) 'enter-check 'visit) s)
  (cond
   [(identifier? s)
    (define id (or alternate-id s))
    (guard-stop
     id ctx s
     (define binding (resolve+shift id (expand-context-phase ctx)
                                    #:ambiguous-value 'ambiguous
                                    #:immediate? #t))
     (log-expand* ctx #:unless (expand-context-only-immediate? ctx) ['resolve id])
     (cond
      [(eq? binding 'ambiguous)
       (raise-ambigious-error id ctx)]
      [(not binding)
       ;; The implicit `#%top` form handles unbound identifiers
       (expand-implicit '#%top (substitute-alternate-id s alternate-id) ctx s)]
      [else
       ;; Variable or form as identifier macro
       (define-values (t insp) (lookup binding ctx id #:in (and alternate-id s)))
       (dispatch t insp s id ctx binding)]))]
   [(and (pair? (syntax-e/no-taint (syntax-disarm s)))
         (identifier? (car (syntax-e/no-taint (syntax-disarm s)))))
    ;; An "application" form that starts with an identifier
    (define disarmed-s (syntax-disarm s #f))
    (define id (or alternate-id (car (syntax-e disarmed-s))))
    (guard-stop
     id ctx s
     (define binding (resolve+shift id (expand-context-phase ctx)
                                    #:ambiguous-value 'ambiguous
                                    #:immediate? #t))
     (log-expand* ctx #:unless (expand-context-only-immediate? ctx) ['resolve id])
     (cond
      [(eq? binding 'ambiguous)
       (raise-ambigious-error id ctx)]
      [(not binding)
       ;; The `#%app` binding might do something with unbound ids
       (expand-implicit '#%app (substitute-alternate-id s alternate-id) ctx id)]
      [else
       ;; Find out whether it's bound as a variable, syntax, or core form
       (define-values (t insp) (lookup binding ctx id #:in (and alternate-id (car (syntax-e disarmed-s)))))
       (cond
        [(variable? t)
         ;; Not as syntax or core form, so use implicit `#%app`
         (expand-implicit '#%app (substitute-alternate-id s alternate-id) ctx id)]
        [else
         ;; Syntax or core form as "application"
         (dispatch t insp s id ctx binding)])]))]
   [(or (pair? (syntax-e (syntax-disarm s)))
        (null? (syntax-e (syntax-disarm s))))
    ;; An "application" form that doesn't start with an identifier, so
    ;; use implicit `#%app`
    (expand-implicit '#%app s ctx #f)]
   [(already-expanded? (syntax-e/no-taint s))
    ;; An expression that is already fully expanded via `local-expand-expression`
    (define ae (syntax-e s))
    (unless (bound-identifier=? (root-expand-context-all-scopes-stx ctx)
                                (already-expanded-all-scopes-stx ae)
                                (expand-context-phase ctx))
      (raise-syntax-error #f
                          (string-append "expanded syntax not in its original lexical context;\n"
                                         " extra bindings or scopes in the current context")
                          (already-expanded-s ae)))
    (define result-s (syntax-track-origin (already-expanded-s ae) s))
    (log-expand* ctx ['tag result-s] ['opaque-expr result-s])
    result-s]
   [else
    ;; Anything other than an identifier or parens triggers the
    ;; implicit `#%datum` form
    (expand-implicit '#%datum s ctx #f)]))

;; Handle an implicit: `#%app`, `#%top`, or `#%datum`
(define (expand-implicit sym s ctx trigger-id)
  (define disarmed-s (syntax-disarm s))
  (define id (datum->syntax disarmed-s sym))
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
   (define-values (t insp) (if b
                               (lookup b ctx id)
                               (values #f #f)))
   (cond
    [(core-form? t)
     (cond
      [(expand-context-only-immediate? ctx)
       (log-expand ctx 'exit-check s)
       s]
      [(and (eq? sym '#%top)
            (eq? (core-form-name t) '#%top)
            (expand-context-preserve-#%expression-and-do-not-add-#%top? ctx))
       ;; Special favor to `local-expand`: call `#%top` form without
       ;; making `#%top` explicit in the form
       (log-expand ctx 'enter-prim s)
       (define result-s ((core-form-expander t) s ctx #t))
       (log-expand* ctx ['exit-prim result-s] ['return result-s])
       result-s]
      [else
       (dispatch t insp (datum->syntax disarmed-s (cons sym disarmed-s) s s) id ctx b)])]
    [(transformer? t)
     (dispatch t insp (datum->syntax disarmed-s (cons sym disarmed-s) s s) id ctx b)]
    [(expand-context-only-immediate? ctx)
     (log-expand ctx 'exit-check s)
     s]
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
;; indicating that the binding is a run-time variable; note that
;; `s` is not disarmed
(define (dispatch t insp s id ctx binding)
  (cond
   [(core-form? t)
    (cond
     [(expand-context-only-immediate? ctx)
      (log-expand ctx 'exit-check s)
      s]
     [else
      (log-expand ctx 'enter-prim s)
      (define result-s ((core-form-expander t) s ctx))
      (log-expand* ctx ['exit-prim result-s] ['return result-s])
      result-s])]
   [(transformer? t)
    (cond
     [(not-in-this-expand-context? t ctx)
      (log-expand ctx 'enter-macro s)
      (define adj-s (avoid-current-expand-context (substitute-alternate-id s id) t ctx))
      (log-expand ctx 'exit-macro s)
      (expand adj-s ctx)]
     [else
      (log-expand* ctx #:when (expand-context-only-immediate? ctx) ['visit s] ['resolves id])
      ;; Apply transformer and expand again
      (define-values (exp-s re-ctx)
        (apply-transformer t insp s id ctx binding))
      (log-expand* ctx #:when (expand-context-only-immediate? ctx) ['return exp-s])
      (cond
       [(expand-context-just-once? ctx) exp-s]
       [else (expand exp-s re-ctx
                     #:alternate-id (and (rename-transformer? t)
                                         (rename-transformer-target t)))])])]
   [(variable? t)
    (cond
     [(expand-context-only-immediate? ctx)
      (log-expand ctx 'exit-check s)
      s]
     [else
      (log-expand ctx 'variable s)
      ;; A reference to a variable expands to itself --- but if the
      ;; binding's frame has a reference record, then register the
      ;; use
      (when (and (local-binding? binding)
                 (reference-record? (binding-frame-id binding)))
        (reference-record-used! (binding-frame-id binding) (local-binding-key binding)))
      ;; If the variable is locally bound, replace the use's scopes with the binding's scopes
      (define result-s (substitute-variable id t #:no-stops? (free-id-set-empty? (expand-context-stops ctx))))
      (log-expand ctx 'return result-s)
      result-s])]
   [else
    ;; Some other compile-time value:
    (raise-syntax-error #f "illegal use of syntax" t)]))

;; Given a macro transformer `t`, apply it --- adding appropriate
;; scopes to represent the expansion step
(define (apply-transformer t insp s id ctx binding)
  (log-expand ctx 'enter-macro s)
  (define disarmed-s (syntax-disarm s))
  (define intro-scope (new-scope 'macro))
  (define intro-s (add-scope disarmed-s intro-scope))
  ;; In a definition context, we need use-site scopes
  (define-values (use-s use-scopes) (maybe-add-use-site-scope intro-s ctx binding))
  ;; Avoid accidental transfer of taint-controlling properties:
  (define cleaned-s (syntax-property-remove (syntax-property-remove use-s 'taint-mode)
                                            'certify-mode))
  ;; Call the transformer; the current expansion context may be needed
  ;; for `syntax-local-....` functions, and we may accumulate scopes from
  ;; definition contexts created by the transformer
  (define def-ctx-scopes (box null))
  (define m-ctx (struct-copy expand-context ctx
                             [current-introduction-scopes (cons intro-scope
                                                                use-scopes)]
                             [def-ctx-scopes def-ctx-scopes]))
  (log-expand ctx 'macro-pre-x use-s)
  (define transformed-s (parameterize ([current-expand-context m-ctx]
                                       [current-namespace (namespace->namespace-at-phase
                                                           (expand-context-namespace ctx)
                                                           (add1 (expand-context-phase ctx)))]
                                       [current-module-code-inspector (or insp (current-module-code-inspector))])
          
                          ((transformer->procedure t) use-s)))
  (log-expand ctx 'macro-post-x transformed-s)
  (unless (syntax? transformed-s)
    (raise-argument-error (syntax-e id)
                          "received value from syntax expander was not syntax"
                          "received" transformed-s))
  (define result-s (flip-scope transformed-s intro-scope))
  ;; In a definition context, we need to add the inside-edge scope to
  ;; any expansion result
  (define post-s (maybe-add-post-expansion-scope result-s ctx))
   ;; Track expansion:
  (define tracked-s (syntax-track-origin post-s s id))
  (log-expand ctx 'enter-macro tracked-s)
  (values
   tracked-s
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
                #:in [in-s #f]
                #:out-of-context-as-variable? [out-of-context-as-variable? #f])
  (binding-lookup b
                  (expand-context-env ctx)
                  (expand-context-lift-envs ctx)
                  (expand-context-namespace ctx)
                  (expand-context-phase ctx)
                  id
                  #:in in-s
                  #:out-of-context-as-variable? out-of-context-as-variable?))

(define-syntax-rule (guard-stop id ctx s otherwise ...)
  (cond
   [(free-id-set-member? (expand-context-stops ctx)
                         (expand-context-phase ctx)
                         id)
    (log-expand* ctx #:unless (expand-context-only-immediate? ctx)
                 ['enter-prim s] ['prim-stop] ['exit-prim s] ['return s])
    s]
   [else
    otherwise ...]))

(define (substitute-alternate-id s alternate-id)
  (cond
   [(not alternate-id) s]
   [(identifier? s) (syntax-rearm (syntax-track-origin alternate-id s) s)]
   [else
    (define disarmed-s (syntax-disarm s))
    (syntax-rearm (syntax-track-origin (datum->syntax
                                        disarmed-s
                                        (cons alternate-id
                                              (cdr (syntax-e disarmed-s)))
                                        s)
                                       s)
                       s)]))

;; ----------------------------------------

;; Expand a sequence of body forms in a definition context
(define (expand-body bodys ctx
                     #:source s #:disarmed-source disarmed-s
                     #:stratified? [stratified? #f]
                     #:track? [track? #f])
  (log-expand ctx 'enter-block)
  ;; The outside-edge scope identifies the original content of the
  ;; definition context
  (define outside-sc (new-scope 'local))
  ;; The inside-edge scope identifiers any form that appears (perhaps
  ;; through macro expansion) in the definition context
  (define inside-sc (new-scope 'intdef))
  (define init-bodys
    (for/list ([body (in-list bodys)])
      (add-scope (add-scope body outside-sc) inside-sc)))
  (log-expand ctx 'block-renames (datum->syntax #f init-bodys) (datum->syntax #f bodys))
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
                                [reference-records (cons frame-id
                                                         (expand-context-reference-records ctx))]
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
             [track-stxs null] ; accumulated syntax for tracking
             [trans-idss null] ; accumulated `define-syntaxes` identifiers that have disappeared
             [dups (make-check-no-duplicate-table)])
    (cond
     [(null? bodys)
      ;; Partial expansion is complete, so finish by rewriting to
      ;; `letrec-values`
      (log-expand body-ctx (if (null? val-idss) 'block->list 'block->letrec))
      (define result-s
        (finish-expanding-body body-ctx frame-id
                               (reverse val-idss) (reverse val-keyss) (reverse val-rhss) (reverse track-stxs)
                               (reverse done-bodys)
                               #:source s #:disarmed-source disarmed-s
                               #:stratified? stratified?
                               #:track? track?))
      (attach-disappeared-transformer-bindings result-s (reverse trans-idss))]
     [else
      (log-expand body-ctx 'next)
      (define exp-body (expand (syntax-disarm (car bodys)) body-ctx))
      (define disarmed-exp-body (syntax-disarm exp-body))
      (case (core-form-sym disarmed-exp-body phase)
        [(begin)
         ;; Splice a `begin` form
         (log-expand body-ctx 'prim-begin)
         (define m (match-syntax disarmed-exp-body '(begin e ...)))
         (define (track e) (syntax-track-origin e exp-body))
         (define splice-bodys (append (map track (m 'e)) (cdr bodys)))
         (log-expand body-ctx 'splice splice-bodys)
         (loop body-ctx
               splice-bodys
               done-bodys
               val-idss
               val-keyss
               val-rhss
               track-stxs
               trans-idss
               dups)]
        [(define-values)
         ;; Found a variable definition; add bindings, extend the
         ;; environment, and continue
         (log-expand body-ctx 'prim-define-values)
         (define m (match-syntax disarmed-exp-body '(define-values (id ...) rhs)))
         (define ids (remove-use-site-scopes (m 'id) body-ctx))
         (log-expand body-ctx 'rename-one (datum->syntax #f (list ids (m 'rhs))))
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
               (cons exp-body (append
                               (for/list ([done-body (in-list done-bodys)])
                                 #f)
                               track-stxs))
               trans-idss
               new-dups)]
        [(define-syntaxes)
         ;; Found a macro definition; add bindings, evaluate the
         ;; compile-time right-hand side, install the compile-time
         ;; values in the environment, and continue
         (log-expand body-ctx 'prim-define-syntaxes)
         (define m (match-syntax disarmed-exp-body '(define-syntaxes (id ...) rhs)))
         (define ids (remove-use-site-scopes (m 'id) body-ctx))
         (log-expand body-ctx 'rename-one (datum->syntax #f (list ids (m 'rhs))))
         (define new-dups (check-no-duplicate-ids ids phase exp-body dups))
         (define counter (root-expand-context-counter ctx))
         (define keys (for/list ([id (in-list ids)])
                        (add-local-binding! id phase counter #:frame-id frame-id)))
         (log-expand body-ctx 'prepare-env)
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
               track-stxs
               (cons ids trans-idss)
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
                 track-stxs
                 trans-idss
                 dups)]
          [else
           ;; Found an expression; accumulate it and continue
           (loop body-ctx
                 (cdr bodys)
                 (cons exp-body done-bodys)
                 val-idss
                 val-keyss
                 val-rhss
                 track-stxs
                 trans-idss
                 dups)])])])))

;; Partial expansion is complete, so assumble the result as a
;; `letrec-values` form and continue expanding
(define (finish-expanding-body body-ctx frame-id
                               val-idss val-keyss val-rhss track-stxs
                               done-bodys
                               #:source s #:disarmed-source disarmed-s
                               #:stratified? stratified?
                               #:track? track?)
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
  (define (finish-bodys track?)
    (define block->list? (null? val-idss))
    (unless block->list? (log-expand body-ctx 'next-group)) ; to go with 'block->letrec
    (cond
     [(null? (cdr done-bodys))
      (define last-ctx (struct-copy expand-context finish-ctx
                                    [reference-records (cdr (expand-context-reference-records finish-ctx))]))
      (define exp-body (expand (car done-bodys) last-ctx))
      (if track?
          (let ([result-s (syntax-track-origin exp-body s)])
            (log-expand body-ctx 'tag result-s)
            result-s)
          exp-body)]
     [else
      (unless block->list? (log-expand body-ctx 'prim-begin))
      (log-expand body-ctx 'enter-list exp-bodys)
      (define exp-bodys (for/list ([body (in-list done-bodys)])
                          (log-expand body-ctx 'next)
                          (expand body finish-ctx)))
      (log-expand body-ctx 'exit-list exp-bodys)
      (rebuild
       #:track? track?
       s disarmed-s
       `(,(datum->syntax s-core-stx 'begin)
         ,@exp-bodys))]))
  (cond
   [(null? val-idss)
    ;; No definitions, so no `letrec-values` wrapper needed:
    (finish-bodys track?)]
   [else
    ;; Roughly, finish expanding the right-hand sides, finish the body
    ;; expression, then add a `letrec-values` wrapper:
    (expand-and-split-bindings-by-reference
     val-idss val-keyss val-rhss track-stxs
     #:split? (not stratified?)
     #:frame-id frame-id #:ctx finish-ctx
     #:source s #:disarmed-source disarmed-s
     #:get-body finish-bodys #:track? track?)]))

;; Roughly, create a `letrec-values` for for the given ids, right-hand sides, and
;; body. While expanding right-hand sides, though, keep track of whether any
;; forward references appear, and if not, generate a `let-values` form, instead,
;; at each binding clause. Similar, end a `letrec-values` form and start a new
;; one if there were forward references up to the clause but not beyond.
(define (expand-and-split-bindings-by-reference idss keyss rhss track-stxs
                                                #:split? split?
                                                #:frame-id frame-id #:ctx ctx 
                                                #:source s #:disarmed-source disarmed-s
                                                #:get-body get-body #:track? track?)
  (define s-core-stx
    (syntax-shift-phase-level core-stx (expand-context-phase ctx)))
  (let loop ([idss idss] [keyss keyss] [rhss rhss] [track-stxs track-stxs]
             [accum-idss null] [accum-rhss null] [accum-track-stxs null]
             [track? track?])
    (cond
     [(null? idss)
      (cond
       [(null? accum-idss) (get-body track?)]
       [else
        (rebuild
         #:track? track?
         s disarmed-s
         `(,(datum->syntax s-core-stx 'letrec-values)
           ,(build-clauses accum-idss accum-rhss accum-track-stxs)
           ,(get-body #f)))])]
     [else
      (log-expand ctx 'next)
      (define ids (car idss))
      (define expanded-rhs (expand (car rhss) (as-named-context ctx ids)))
      (define track-stx (car track-stxs))
      
      (define local-or-forward-references? (reference-record-forward-references? frame-id))
      (reference-record-bound! frame-id (car keyss))
      (define forward-references? (reference-record-forward-references? frame-id))
      
      (cond
       [(and (not local-or-forward-references?)
             split?)
        (unless (null? accum-idss) (error "internal error: accumulated ids not empty"))
        (rebuild
         #:track? track?
         s disarmed-s
         `(,(datum->syntax s-core-stx 'let-values)
           (,(build-clause ids expanded-rhs track-stx))
           ,(loop (cdr idss) (cdr keyss) (cdr rhss) (cdr track-stxs)
                  null null null
                  #f)))]
       [(and (not forward-references?)
             (or split? (null? (cdr idss))))
        (rebuild
         #:track? track?
         s disarmed-s
         `(,(datum->syntax s-core-stx 'letrec-values)
           ,(build-clauses (cons ids accum-idss)
                           (cons expanded-rhs accum-rhss)
                           (cons track-stx accum-track-stxs))
           ,(loop (cdr idss) (cdr keyss) (cdr rhss) (cdr track-stxs)
                  null null null
                  #f)))]
       [else
        (loop (cdr idss) (cdr keyss) (cdr rhss) (cdr track-stxs)
              (cons ids accum-idss) (cons expanded-rhs accum-rhss) (cons track-stx accum-track-stxs)
              track?)])])))

(define (build-clauses accum-idss accum-rhss accum-track-stxs)
  (map build-clause
       (reverse accum-idss)
       (reverse accum-rhss)
       (reverse accum-track-stxs)))

(define (build-clause ids rhs track-stx)
  (define clause (datum->syntax #f `[,ids ,rhs]))
  (if track-stx
      (syntax-track-origin clause track-stx)
      clause))

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
                              #:begin-form? [begin-form? #f]
                              #:lift-key [lift-key (generate-lift-key)])
  (define context (expand-context-context ctx))
  (define phase (expand-context-phase ctx))
  (define local? (not begin-form?)) ;; see "[*]" below
  ;; Expand `s`, but loop to handle lifted expressions
  (let loop ([s s])
    (define lift-env (and local? (box empty-env)))
    (define lift-ctx (make-lift-context
                      (if local?
                          (make-local-lift lift-env (root-expand-context-counter ctx))
                          (make-top-level-lift ctx))
                      #:module*-ok? (and (not local?) (eq? context 'module))))
    (define capture-ctx (struct-copy expand-context ctx
                                     [lift-key #:parent root-expand-context lift-key]
                                     [lifts lift-ctx]
                                     [lift-envs (if local?
                                                    (cons lift-env
                                                          (expand-context-lift-envs ctx))
                                                    (expand-context-lift-envs ctx))]
                                     [module-lifts (if (or local?
                                                           (not (memq context '(top-level module))))
                                                       (expand-context-module-lifts ctx)
                                                       lift-ctx)]))
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
      (log-expand ctx 'lift-loop with-lifts-s)
      (if expand-lifts?
          (loop with-lifts-s)
          with-lifts-s)])))

;; [*] Although `(memq context '(top-level module))` makes more sense
;;     than `(not begin-form?)`, the latter was used historically; the
;;     implementation of `typed/require` currently depends on that
;;     choice, because it expands in 'expression mode to obtain forms
;;     that are splcied into a module context --- leading to an
;;     out-of-context definition error if the historical choice is not
;;     preserved.

;; Expand `s` as a compile-time expression relative to the current
;; expansion context
(define (expand-transformer s ctx
                            #:context [context 'expression]
                            #:begin-form? [begin-form? #f]
                            #:expand-lifts? [expand-lifts? #t]
                            #:lift-key [lift-key (generate-lift-key)])
  (define phase (add1 (expand-context-phase ctx)))
  (define ns (namespace->namespace-at-phase (expand-context-namespace ctx)
                                            phase))
  (namespace-visit-available-modules! ns phase)
  (define trans-ctx (struct-copy expand-context ctx
                                 [context context]
                                 [scopes null]
                                 [phase phase]
                                 [namespace ns]
                                 [env empty-env]
                                 [only-immediate? #f]
                                 [post-expansion-scope #:parent root-expand-context #f]))
  (expand/capture-lifts s trans-ctx
                        #:expand-lifts? expand-lifts?
                        #:begin-form? begin-form?
                        #:lift-key lift-key))

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
                        (parameterize ([current-expand-context ctx]
                                       [current-namespace ns])
                          (eval-top compiled ns)))
      list))
  (unless (= (length vals) (length ids))
    (error "wrong number of results (" (length vals) "vs." (length ids) ")"
           "from" s))
  vals)

;; ----------------------------------------

;; A helper for forms to reconstruct syntax while preserving source
;; locations, properties, and arming; if `track?` is #f, then don't keep
;; properties, because we've kept them in a surrounding form
(define (rebuild orig-s disarmed-orig-s new
                 #:track? [track? #t])
  (syntax-rearm (datum->syntax disarmed-orig-s new orig-s (and track? orig-s))
                orig-s))

(define (attach-disappeared-transformer-bindings s trans-idss)
   (cond
    [(null? trans-idss) s]
    [else
     (syntax-property s
                      'disappeared-binding
                      (append (apply append trans-idss)
                              (or (syntax-property s 'disappeared-binding)
                                  null)))]))
