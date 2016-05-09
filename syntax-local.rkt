#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "free-id-set.rkt"
         "core.rkt"
         "expand-context.rkt"
         "expand.rkt")

(provide default-phase
         syntax-local-value
         local-expand
         syntax-local-make-definition-context
         syntax-local-bind-syntaxes
         syntax-local-identifier-as-binding)

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

(define (syntax-local-value id [failure-thunk #f])
  (define ctx (get-current-expand-context 'syntax-local-value))
  (define phase (expand-context-phase ctx))
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
     [else v])]))

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

(struct env-mixin (sym
                   value
                   cache)) ; caches addition of binding to an existing environment

(define (syntax-local-make-definition-context [parent-ctx #f] [add-scope? #t])
  (void (get-current-expand-context 'syntax-local-make-definition-context))
  (define sc (new-scope))
  (internal-definition-context sc add-scope? (box null)))

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
  (define syms (for/list ([id (in-list ids)])
                 (define pre-id (remove-use-site-scopes (flip-introduction-scopes id ctx)
                                                        ctx))
                 (define intdef-id (add-intdef-scopes pre-id intdef #:always? #t))
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
  (set-box! env-mixins (append (for/list ([sym (in-list syms)]
                                          [val (in-list vals)])
                                 (env-mixin sym val (make-weak-hasheq)))
                               (unbox env-mixins))))

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

(define (add-intdef-scopes s intdefs #:always? [always? #f])
  (for/fold ([s s]) ([intdef (in-intdefs intdefs)]
                     #:when (or always?
                                (internal-definition-context-add-scope? intdef)))
    (add-scope s (internal-definition-context-scope intdef))))

;; ----------------------------------------

(define (syntax-local-identifier-as-binding id)
  (unless (identifier? id)
    (raise-argument-error 'syntax-local-identifier-as-binding "identifier?" id))
  (define ctx (get-current-expand-context 'syntax-local-identifier-as-binding))
  (remove-use-site-scopes id ctx))
