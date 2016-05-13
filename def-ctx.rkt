#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "syntax-local.rkt")

(provide add-intdef-scopes
         add-intdef-bindings
         
         internal-definition-context?
         syntax-local-make-definition-context
         syntax-local-bind-syntaxes
         internal-definition-context-binding-identifiers
         internal-definition-context-introduce
         internal-definition-context-seal
         identifier-remove-from-definition-context)

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
  (define sc (new-scope 'intdef))
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
