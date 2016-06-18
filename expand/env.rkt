#lang racket/base
(require "../common/memo.rkt"
         "../syntax/syntax.rkt"
         "../syntax/error.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../common/phase.rkt"
         "../syntax/binding.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "../common/module-path.rkt")

(provide empty-env
         env-extend
         
         variable
         (struct-out core-form)
         
         transformer? transformer->procedure
         variable?

         (struct-out local-variable)
         substitute-variable

         binding-lookup)

;; ----------------------------------------

;; An expansion environment maps keys to either `variable` or a
;; compile-time value:
(define empty-env #hasheq())
(define (env-extend env key val)
  (hash-set env key val))

;; `variable` is a token to represent a binding to a run-time variable
(define variable (gensym 'variable))
(define (variable? t) (or (eq? t variable)
                          (local-variable? t)))

;; A `local-variable` records a binding identifier, so that a
;; reference can be replaced with the binding identifier
(struct local-variable (id))

;; If a variable binding corresponds to a local binding, substitute
;; the binding identifier in place of the original reference
(define (substitute-variable id t #:no-stops? no-stops?)
  (if (and no-stops? (local-variable? t))
      (let ([bind-id (local-variable-id t)])
        ;; Keep source locations and properties of original reference:
        (syntax-rearm (datum->syntax (syntax-disarm bind-id) (syntax-e bind-id) id id)
                      id))
      id))

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
(struct core-form (expander name) #:transparent)

;; ---------------------------------------- 

;; Returns: `variable` or a compile-time value
;;          #f or (for a transformer) an inspector for the defining module
(define (binding-lookup b env lift-envs ns phase id
                        #:out-of-context-as-variable? [out-of-context-as-variable? #f])
  (cond
   [(module-binding? b)
    (define at-phase (- phase (module-binding-phase b)))
    (define-values (mi m-ns)
      (if (top-level-module-path-index? (module-binding-module b))
          (values #f ns)
          (let ([mi
                 (namespace->module-instance ns
                                             (module-path-index-resolve
                                              (module-binding-module b))
                                             at-phase
                                             #:check-available-at-phase-level (module-binding-phase b)
                                             #:unavailable-callback
                                             (lambda ()
                                               (raise-syntax-error
                                                #f
                                                (format (string-append "module mismatch;\n"
                                                                       " attempted to use a module that is not available\n"
                                                                       "  possible cause:\n"
                                                                       "   using (dynamic-require .... #f)\n"
                                                                       "   but need (dynamic-require .... 0)\n"
                                                                       "  module: ~s\n"
                                                                       "  phase: ~s")
                                                        (module-binding-module b)
                                                        (phase+ at-phase (module-binding-phase b)))
                                                id)))])
            (values mi (and mi (module-instance-namespace mi))))))
    (unless m-ns
      (error 'expand
             (string-append "namespace mismatch; cannot locate module instance\n"
                            "  X: ~s\n"
                            "  module: ~s\n"
                            "  use phase: ~a\n"
                            "  definition phase: ~a\n"
                            "  for identifier: ~s")
             (module-binding-module b)
             phase
             (module-binding-phase b)
             id))
    (check-taint id)
    (define t (namespace-get-transformer m-ns (module-binding-phase b) (module-binding-sym b)
                                         variable))
    (values t (and mi (module-instance-inspector mi)))]
   [(local-binding? b)
    (define t (hash-ref env (local-binding-key b) missing))
    (cond
     [(eq? t missing)
      (values (or
               ;; check in lift envs, if any
               (for/or ([lift-env (in-list lift-envs)])
                 (hash-ref (unbox lift-env) (local-binding-key b) #f))
               (if out-of-context-as-variable?
                   variable
                   (error "identifier used out of context:" id)))
              #f)]
     [else
      (check-taint id)
      (values t #f)])]
   [else (error "internal error: unknown binding for lookup:" b)]))

;; ----------------------------------------

(define (check-taint id)
  (when (syntax-tainted? id)
    (raise-syntax-error #f
                        "cannot use identifier tainted by macro transformation"
                        id)))
