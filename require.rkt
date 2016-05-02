#lang racket/base
(require "stx.rkt"
         "scope.rkt"
         "namespace.rkt")

(provide stx-context-require!
         stx-context-require/expansion-time!)

(define (bind-all-exports! in-stx phase-level ns module-name filter)
  (define m (namespace->module ns module-name))
  (unless m
    (error "module not declared:" module-name))
  (define (bind-all exports)
    (for ([(export-phase-level exports) (in-hash exports)])
      (define phase (+ phase-level export-phase-level))
      (for ([(sym binding) (in-hash exports)])
        (let-values ([(sym) (filter sym export-phase-level)])
          (when sym
            (add-binding! (datum->syntax in-stx sym) binding phase))))))
  (bind-all (module-variable-exports m))
  (bind-all (module-transformer-exports m)))

(define (default-filter sym phase-level)
  sym)

(define (stx-context-require/expansion-time! in-stx phase-level ns module-name
                                             #:filter [filter default-filter])
  (bind-all-exports! in-stx phase-level ns module-name filter)
  (namespace-module-visit! ns module-name phase-level 0))
  
(define (stx-context-require! in-stx phase-level ns module-name)
  (stx-context-require/expansion-time! in-stx phase-level ns module-name)
  (namespace-module-instantiate! ns module-name phase-level 0))
