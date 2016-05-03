#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "namespace.rkt")

(provide syntax-context-require!
         syntax-context-require/expansion-time!)

(define (bind-all-exports! in-stx phase-shift ns module-name filter)
  (define m (namespace->module ns module-name))
  (unless m
    (error "module not declared:" module-name))
  (for ([(export-phase-level exports) (in-hash (module-provides m))])
    (define phase (phase+ phase-shift export-phase-level))
    (for ([(sym binding) (in-hash exports)])
      (define from-mod (module-binding-module binding))
      (define b (struct-copy module-binding binding
                             [module (if (eq? from-mod 'self)
                                         module-name
                                         from-mod)]
                             [nominal-module module-name]
                             [nominal-phase export-phase-level]
                             [nominal-sym sym]
                             [nominal-import-phase phase-shift]))
      (let-values ([(sym) (filter b)])
        (when sym
          (add-binding! (datum->syntax in-stx sym) b phase))))))

(define (default-filter binding)
  (module-binding-nominal-sym binding))

(define (syntax-context-require/expansion-time! in-stx phase-shift ns module-name
                                                #:filter [filter default-filter])
  (bind-all-exports! in-stx phase-shift ns module-name filter)
  (namespace-module-visit! ns module-name phase-shift))
  
(define (syntax-context-require! in-stx phase-shift ns module-name)
  (syntax-context-require/expansion-time! in-stx phase-shift ns module-name)
  (namespace-module-instantiate! ns module-name phase-shift))
