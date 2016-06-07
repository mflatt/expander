#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "core.rkt"
         "free-id-set.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "syntax-local.rkt"
         "def-ctx.rkt"
         "already-expanded.rkt")

(provide local-expand
         local-expand/capture-lifts
         local-transformer-expand
         local-transformer-expand/capture-lifts
         syntax-local-expand-expression)

(define (local-expand s context stop-ids [intdefs #f])
  (do-local-expand 'local-expand s context stop-ids intdefs))

(define (local-expand/capture-lifts s context stop-ids [intdefs #f])
  (do-local-expand 'local-expand s context stop-ids intdefs
                   #:capture-lifts? #t))

(define (local-transformer-expand s context stop-ids [intdefs #f])
  (do-local-expand 'local-expand s context stop-ids intdefs
                   #:as-transformer? #t))

(define (local-transformer-expand/capture-lifts s context stop-ids [intdefs #f])
  (do-local-expand 'local-expand s context stop-ids intdefs
                   #:as-transformer? #t
                   #:capture-lifts? #t))

(define (syntax-local-expand-expression s)
  (define exp-s (do-local-expand 'local-expand s 'expression null #f))
  (values exp-s (already-expanded
                 exp-s
                 (root-expand-context-all-scopes-stx (current-expand-context)))))

;; ----------------------------------------

(define (do-local-expand who s context stop-ids [intdefs #f]
                         #:capture-lifts? [capture-lifts? #t]
                         #:as-transformer? [as-transformer? #f])
  (unless (syntax? s)
    (raise-argument-error who "syntax?" s))
  (unless (or (list? context)
              (memq context (if as-transformer?
                                '(expression top-level)
                                '(expression top-level module module-begin))))
    (raise-argument-error who
                          (if as-transformer?
                              "(or/c 'expression 'top-level list?)"
                              "(or/c 'expression 'top-level 'module 'module-begin list?)")
                          context))
  (unless (or (not stop-ids)
              (and (list? stop-ids)
                   (andmap identifier? stop-ids)))
    (raise-argument-error who "(or/c (listof identifier?) #f)" stop-ids))
  (unless (or (not intdefs)
              (internal-definition-context? intdefs)
              (and (list? intdefs) (andmap internal-definition-context? intdefs)))
    (raise-argument-error who
                          "(or/c #f internal-definitionc-context? (listof internal-definitionc-context?))" 
                          intdefs))
  (define ctx (get-current-expand-context who))
  (define same-kind? (or (eq? context
                              (expand-context-context ctx))
                         (and (list? context)
                              (list? (expand-context-context ctx)))))
  (define phase (if as-transformer?
                    (add1 (expand-context-phase ctx))
                    (expand-context-phase ctx)))
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
                                                      phase
                                                      phase))
                              stop-ids]
                             [else (append stop-ids
                                           (for/list ([sym (in-list auto-stop-syms)])
                                             (datum->syntax p-core-stx sym)))])))
  (define local-ctx (struct-copy expand-context ctx
                                 [context context]
                                 [env (add-intdef-bindings (expand-context-env ctx)
                                                           intdefs)]
                                 [use-site-scopes
                                  #:parent root-expand-context
                                  (and (list? context)
                                       (or (root-expand-context-use-site-scopes ctx)
                                           (box null)))]
                                 [frame-id #:parent root-expand-context
                                           (cond
                                            [same-kind? (root-expand-context-frame-id ctx)]
                                            [(pair? intdefs)
                                             (internal-definition-context-frame-id (car intdefs))]
                                            [else #f])]
                                 [post-expansion-scope
                                  #:parent root-expand-context
                                  (and (and same-kind?
                                            (memq context '(module top-level))
                                            (root-expand-context-post-expansion-scope ctx)))]
                                 [scopes
                                  (append (if (expand-context-def-ctx-scopes ctx)
                                              (unbox (expand-context-def-ctx-scopes ctx))
                                              null)
                                          (expand-context-scopes ctx))]
                                 [only-immediate? (not stop-ids)]
                                 [stops (free-id-set phase (or all-stop-ids null))]
                                 [current-introduction-scopes null]
                                 [all-scopes-stx #:parent root-expand-context
                                                 (add-intdef-scopes
                                                  (root-expand-context-all-scopes-stx ctx)
                                                  intdefs)]))
  (define input-s (add-intdef-scopes (flip-introduction-scopes s ctx) intdefs))
  (define output-s (cond
                    [(and as-transformer? capture-lifts?)
                     (expand-transformer input-s local-ctx
                                         #:begin-form? #t)]
                    [as-transformer?
                     (expand-transformer input-s local-ctx
                                         #:begin-form? (eq? 'top-level context))]
                    [capture-lifts?
                     (expand/capture-lifts input-s local-ctx
                                           #:begin-form? #t)]
                    [else
                     (expand input-s local-ctx)]))
  (flip-introduction-scopes output-s ctx))
