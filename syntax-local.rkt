#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "expand-context.rkt"
         "expand.rkt")

(provide default-phase
         syntax-local-value)

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
