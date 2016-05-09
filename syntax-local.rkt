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
         local-expand)

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

(define (local-expand s context stop-ids)
  (unless (syntax? s)
    (raise-argument-error 'local-expand "syntax?" s))
  (unless (or (list? context)
              (memq context '(expression top-level module module-begin)))
    (raise-argument-error 'local-expand "(or/c 'expression 'top-level 'module 'module-begin list?)" context))
  (unless (or (not stop-ids)
              (and (list? stop-ids)
                   (andmap identifier? stop-ids)))
    (raise-argument-error 'local-expand "(or/c (listof identifier?) #f)" stop-ids))
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
                                     [use-site-scopes
                                      (and (list? context)
                                           (or (expand-context-use-site-scopes ctx)
                                               (box null)))]
                                     [post-expansion-scope
                                      (and (and same-kind?
                                                (memq context '(module top-level))
                                                (expand-context-post-expansion-scope ctx)))]
                                     [only-immediate? (not stop-ids)]
                                     [stops (free-id-set phase (or all-stop-ids null))])))
  (expand s local-ctx))
