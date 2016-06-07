#lang racket/base
(require "memo.rkt"
         "syntax.rkt"
         "scope.rkt"
         "phase.rkt"
         "binding.rkt"
         "namespace.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "module-path.rkt")

(provide empty-env
         env-extend
         
         variable
         (struct-out core-form)
         
         transformer? transformer->procedure
         variable?
         
         binding-lookup)

;; ----------------------------------------

;; An expansion environment maps keys to either `variable` or a
;; compile-time value:
(define empty-env #hasheq())
(define (env-extend env key val)
  (hash-set env key val))

;; `variable` is a token to represent a binding to a run-time variable
(define variable (gensym 'variable))
(define (variable? t) (eq? t variable))

;; `missing` is a token to represent the absence of a binding; a
;; distinct token is needed so that it's distinct from all compile-time
;; values
(define missing (gensym 'missing))
(define (missing? t) (eq? t missing))

;; A subset of compile-time values are macro transformers
(define (transformer? t) (or (procedure? t)
                             (set!-transformer? t)
                             (rename-transformer? t)))
(define (transformer->procedure t)
  (cond
   [(set!-transformer? t) (set!-transformer-procedure t)]
   [(rename-transformer? t) (lambda (s) s)] ; "expansion" handled via #:alternate-id
   [else t]))

;; A subset of compile-time values are primitive forms
(struct core-form (expander) #:transparent)

;; ---------------------------------------- 

;; Returns `variable` or a compile-time value
(define (binding-lookup b env lift-envs ns phase id
                        #:out-of-context-as-variable? [out-of-context-as-variable? #f])
  (cond
   [(module-binding? b)
    (define at-phase (- phase (module-binding-phase b)))
    (define m (if (top-level-module-path-index? (module-binding-module b))
                  ns
                  (namespace->module-namespace ns
                                               (module-path-index-resolve
                                                (module-binding-module b))
                                               at-phase)))
    (unless m
      (error "namespace mismatch: cannot locate module"
             (module-binding-module b)
             at-phase
             "for identifier"
             id))
    (namespace-get-transformer m (module-binding-phase b) (module-binding-sym b)
                               variable)]
   [(local-binding? b)
    (define t (hash-ref env (local-binding-key b) missing))
    (cond
     [(eq? t missing)
      (or
       ;; check in lift envs, if any
       (for/or ([lift-env (in-list lift-envs)])
         (hash-ref (unbox lift-env) (local-binding-key b) #f))
       (if out-of-context-as-variable?
           variable
           (error "identifier used out of context:" id)))]
     [else t])]
   [else (error "internal error: unknown binding for lookup:" b)]))
