#lang racket/base
(require racket/set
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "free-id-set.rkt"
         "core.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "rename-trans.rkt")

(provide default-phase
         
         syntax-local-context
         syntax-local-introduce
         syntax-local-identifier-as-binding
         syntax-local-phase-level
         
         make-syntax-introducer
         make-syntax-delta-introducer
         
         syntax-local-value
         syntax-local-value/immediate
         
         local-expand
         
         internal-definition-context?
         syntax-local-make-definition-context
         syntax-local-bind-syntaxes
         internal-definition-context-binding-identifiers
         internal-definition-context-introduce
         internal-definition-context-seal
         identifier-remove-from-definition-context)

;; `bound-identifier=?` and `free-identifier=?` use the current
;; context to determine the default phase
(define (default-phase)
  (define ctx (current-expand-context))
  (if ctx
      (expand-context-phase ctx)
      0))

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
  (default-phase))

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

(define (make-syntax-delta-introducer ext-s base-s [phase (default-phase)])
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
      (define v (binding-lookup b
                                (expand-context-env ctx)
                                (expand-context-namespace ctx)
                                phase
                                id))
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

(define (local-expand s context stop-ids [intdefs #f])
  (unless (syntax? s)
    (raise-argument-error 'local-expand "syntax?" s))
  (unless (or (list? context)
              (memq context '(expression top-level module module-begin)))
    (raise-argument-error 'local-expand "(or/c 'expression 'top-level 'module 'module-begin list?)" context))
  (unless (or (not stop-ids)
              (and (list? stop-ids)
                   (andmap identifier? stop-ids)))
    (raise-argument-error 'local-expand "(or/c (listof identifier?) #f)" stop-ids))
  (unless (or (not intdefs)
              (internal-definition-context? intdefs)
              (and (list? intdefs) (andmap internal-definition-context? intdefs)))
    (raise-argument-error 'local-expand
                          "(or/c #f internal-definitionc-context? (listof internal-definitionc-context?))" 
                          intdefs))
  (define ctx (get-current-expand-context 'local-expand))
  (define same-kind? (or (eq? context
                              (expand-context-context ctx))
                         (and (list? context)
                              (list? (expand-context-context ctx)))))
  (define phase (expand-context-phase ctx))
  (define p-core-stx (syntax-shift-phase-level core-stx phase))
  (define auto-stop-syms '(begin quote set! lambda case-lambda let-values letrec-values
                           if begin0 with-continuation-mark letrec-syntaxes+values
                           #%app #%expression #%top #%variable-reference))
  (define all-stop-ids (and stop-ids
                            (cond
                             [(null? stop-ids) stop-ids]
                             [(and (= 1 (length stop-ids))
                                   (free-identifier=? (car stop-ids)
                                                      (datum->syntax p-core-stx 'module*)
                                                      phase))
                              stop-ids]
                             [else (append stop-ids
                                           (for/list ([sym (in-list auto-stop-syms)])
                                             (datum->syntax p-core-stx sym)))])))
  (define local-ctx (if same-kind?
                        ctx
                        (struct-copy expand-context ctx
                                     [context context]
                                     [env (add-intdef-bindings (expand-context-env ctx)
                                                               intdefs)]
                                     [use-site-scopes
                                      (and (list? context)
                                           (or (expand-context-use-site-scopes ctx)
                                               (box null)))]
                                     [post-expansion-scope
                                      (and (and same-kind?
                                                (memq context '(module top-level))
                                                (expand-context-post-expansion-scope ctx)))]
                                     [only-immediate? (not stop-ids)]
                                     [stops (free-id-set phase (or all-stop-ids null))]
                                     [current-introduction-scopes null])))
  (define input-s (add-intdef-scopes (flip-introduction-scopes s ctx) intdefs))
  (define output-s (expand input-s local-ctx))
  (flip-introduction-scopes output-s ctx))

;; ----------------------------------------

(struct internal-definition-context (scope         ; scope that represents the context
                                     add-scope?    ; whether the scope is auto-added for expansion
                                     env-mixins))  ; bindings for this context: box of list of mix-binding

(struct env-mixin (id
                   sym
                   value
                   cache)) ; caches addition of binding to an existing environment

;; syntax-local-make-definition-context
(define (syntax-local-make-definition-context [parent-ctx #f] [add-scope? #t])
  (void (get-current-expand-context 'syntax-local-make-definition-context))
  (define sc (new-scope))
  (internal-definition-context sc add-scope? (box null)))

;; syntax-local-bind-syntaxes
(define (syntax-local-bind-syntaxes ids s intdef)
  (unless (and (list? ids)
               (andmap identifier? ids))
    (raise-argument-error 'syntax-local-bind-syntaxes "(listof identifier?)" ids))
  (unless (or (not s) (syntax? s))
    (raise-argument-error 'syntax-local-bind-syntaxes "(or/c syntax? #f)" s))
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'syntax-local-bind-syntaxes "internal-definition-context?" intdef))
  (define ctx (get-current-expand-context 'local-expand))
  (define phase (expand-context-phase ctx))
  (define intdef-env (add-intdef-bindings (expand-context-env ctx)
                                          intdef))
  (define intdef-ids (for/list ([id (in-list ids)])
                       (define pre-id (remove-use-site-scopes (flip-introduction-scopes id ctx)
                                                              ctx))
                       (add-intdef-scopes pre-id intdef #:always? #t)))
  (define syms (for/list ([intdef-id (in-list intdef-ids)])
                 (add-local-binding! intdef-id phase)))
  (define vals
    (cond
     [s
      (define input-s (flip-introduction-scopes (add-intdef-scopes s intdef #:always? #t)
                                                ctx))
      (define tmp-env (for/fold ([env intdef-env]) ([sym (in-list syms)])
                        (hash-set env sym variable)))
      (eval-for-syntaxes-binding input-s ids (struct-copy expand-context ctx
                                                          [env tmp-env]))]
     [else
      (for/list ([id (in-list ids)]) variable)]))
  (define env-mixins (internal-definition-context-env-mixins intdef))
  (set-box! env-mixins (append (for/list ([intdef-id (in-list intdef-ids)]
                                          [sym (in-list syms)]
                                          [val (in-list vals)])
                                 (env-mixin intdef-id sym val (make-weak-hasheq)))
                               (unbox env-mixins))))

;; internal-definition-context-binding-identifiers
(define (internal-definition-context-binding-identifiers intdef)
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-binding-identifiers "internal-definition-context?" intdef))
  (for/list ([env-mixin (in-list (internal-definition-context-env-mixins intdef))])
    (env-mixin-id env-mixin)))

;; internal-definition-context-introduce
(define (internal-definition-context-introduce intdef s [mode 'flip])
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-introduce "internal-definition-context?" intdef))
  (unless (syntax? s)
    (raise-argument-error 'internal-definition-context-introduce "syntax?" s))
  (add-intdef-scopes s intdef
                     #:action (case mode
                                [(add) add-scopes]
                                [(remove) remove-scopes]
                                [(flip) flip-scope]
                                [else (raise-argument-error
                                       internal-definition-context-introduce
                                       "(or/c 'add 'remove 'flip)"
                                       mode)])))

;; internal-definition-context-seal
(define (internal-definition-context-seal intdef) 
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-seal "internal-definition-context?" intdef))
  (void))

;; identifier-remove-from-definition-context
(define (identifier-remove-from-definition-context id intdef)
  (unless (identifier? id)
    (raise-argument-error 'identifier-remove-from-definition-context "identifier?" id))
  (unless (or (internal-definition-context? intdef)
              (and (list? intdef)
                   (andmap internal-definition-context? intdef)))
    (raise-argument-error 'identifier-remove-from-definition-context
                          "(or/c internal-definition-context? (listof internal-definition-context?))"
                          intdef))
  (for/fold ([id id]) ([intdef (in-intdefs intdef)])
    (internal-definition-context-introduce intdef id 'remove)))

;; Sequence for intdefs provided to `local-expand`
(define (in-intdefs intdefs)
  (cond
   [(not intdefs) (in-list null)]
   [(list? intdefs) (in-list (reverse intdefs))]
   [else (in-value intdefs)]))

(define (add-intdef-bindings env intdefs)
  (for/fold ([env env]) ([intdef (in-intdefs intdefs)])
    (define env-mixins (unbox (internal-definition-context-env-mixins intdef)))
    (let loop ([env env] [env-mixins env-mixins])
      (cond
       [(null? env-mixins) env]
       [else
        (define env-mixin (car env-mixins))
        (or (hash-ref (env-mixin-cache env-mixin) env #f)
            (let ([new-env (env-extend (loop env (cdr env-mixins))
                                       (env-mixin-sym env-mixin)
                                       (env-mixin-value env-mixin))])
              (hash-set! (env-mixin-cache env-mixin) env new-env)
              new-env))]))))

(define (add-intdef-scopes s intdefs
                           #:always? [always? #f]
                           #:action [action add-scope])
  (for/fold ([s s]) ([intdef (in-intdefs intdefs)]
                     #:when (or always?
                                (internal-definition-context-add-scope? intdef)))
    (action s (internal-definition-context-scope intdef))))
