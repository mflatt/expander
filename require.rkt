#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "namespace.rkt")

(provide syntax-context-require!
         syntax-context-require/expansion-time!)

(define (bind-all-provides! in-stx phase-shift ns module-name filter)
  (define m (namespace->module ns module-name))
  (unless m
    (error "module not declared:" module-name))
  (for ([(provide-phase-level provides) (in-hash (module-provides m))])
    (define phase (phase+ phase-shift provide-phase-level))
    (for ([(sym binding) (in-hash provides)])
      (define from-mod (module-binding-module binding))
      (define b (struct-copy module-binding binding
                             [module (if (eq? from-mod 'self)
                                         module-name
                                         from-mod)]
                             [nominal-module module-name]
                             [nominal-phase provide-phase-level]
                             [nominal-sym sym]
                             [nominal-require-phase phase-shift]))
      (let-values ([(sym) (filter b)])
        (when sym
          (add-binding! (datum->syntax in-stx sym) b phase))))))

(define (default-filter binding)
  (module-binding-nominal-sym binding))

(define (syntax-context-require/expansion-time! in-stx phase-shift ns module-name
                                                #:filter [filter default-filter])
  (bind-all-provides! in-stx phase-shift ns module-name filter)
  (namespace-module-visit! ns module-name phase-shift))
  
(define (syntax-context-require! in-stx phase-shift ns module-name)
  (syntax-context-require/expansion-time! in-stx phase-shift ns module-name)
  (namespace-module-instantiate! ns module-name phase-shift))
