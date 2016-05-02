#lang racket/base
(require "stx.rkt"
         "scope.rkt"
         "binding.rkt"
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
        (define b (struct-copy module-binding binding
                               [nominal-module module-name]
                               [nominal-phase export-phase-level]
                               [nominal-sym sym]
                               [nominal-import-phase phase-level]))
        (let-values ([(sym) (filter b)])
          (when sym
            (add-binding! (datum->syntax in-stx sym) b phase))))))
  (bind-all (module-variable-exports m))
  (bind-all (module-transformer-exports m)))

(define (default-filter binding)
  (module-binding-nominal-sym binding))

(define (stx-context-require/expansion-time! in-stx phase-level ns module-name
                                             #:filter [filter default-filter])
  (bind-all-exports! in-stx phase-level ns module-name filter)
  (namespace-module-visit! ns module-name phase-level 0))
  
(define (stx-context-require! in-stx phase-level ns module-name)
  (stx-context-require/expansion-time! in-stx phase-level ns module-name)
  (namespace-module-instantiate! ns module-name phase-level 0))
