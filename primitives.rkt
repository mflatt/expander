#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         (rename-in "scope.rkt"
                    [bound-identifier=? raw:bound-identifier=?])
         "namespace.rkt"
         (rename-in "binding.rkt"
                    [free-identifier=? raw:free-identifier=?])
         "core.rkt"
         "syntax-local.rkt")

(define (bound-identifier=? a b [phase (default-phase)])
  (unless (identifier? a)
    (raise-argument-error 'bound-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'bound-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:bound-identifier=? a b phase))

(define (free-identifier=? a b [phase (default-phase)])
  (unless (identifier? a)
    (raise-argument-error 'free-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'free-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'free-identifier=? "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:free-identifier=? a b phase))

;; Register core primitives:
(define-syntax-rule (add-core-primitives! id ...)
  (begin
    (add-core-primitive! 'id id)
    ...))

(add-core-primitives! syntax-e
                      datum->syntax
                      bound-identifier=?
                      free-identifier=?
                      
                      syntax-local-context
                      syntax-local-introduce
                      syntax-local-identifier-as-binding
                      syntax-local-phase-level

                      make-syntax-introducer
                      make-syntax-delta-introducer
                      
                      syntax-local-value
                      local-expand
                      
                      internal-definition-context?
                      syntax-local-make-definition-context
                      syntax-local-bind-syntaxes
                      internal-definition-context-binding-identifiers
                      internal-definition-context-introduce
                      internal-definition-context-seal
                      identifier-remove-from-definition-context

                      ;; This list will need to be a lot longer...
                      list cons car cdr null? map
                      values 
                      error
                      println
                      random
                      +)
