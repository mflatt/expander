#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         (rename-in "scope.rkt"
                    [bound-identifier=? raw:bound-identifier=?])
         "namespace.rkt"
         (rename-in "binding.rkt"
                    [free-identifier=? raw:free-identifier=?])
         "core.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "syntax-local.rkt"
         "def-ctx.rkt"
         "local-expand.rkt")

(define (bound-identifier=? a b [phase (syntax-local-phase-level)])
  (unless (identifier? a)
    (raise-argument-error 'bound-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'bound-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:bound-identifier=? a b phase))

(define (free-identifier=? a b [phase (syntax-local-phase-level)])
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
                      
                      syntax-transforming?
                      syntax-transforming-with-lifts?
                      syntax-transforming-module-expression?
                      syntax-local-transforming-module-provides?
                      
                      syntax-local-context
                      syntax-local-introduce
                      syntax-local-identifier-as-binding
                      syntax-local-phase-level

                      make-syntax-introducer
                      make-syntax-delta-introducer
                      
                      syntax-local-value
                      syntax-local-value/immediate
                      
                      syntax-local-lift-expression
                      syntax-local-lift-values-expression
                      syntax-local-lift-context
                      
                      syntax-local-lift-module
                      
                      syntax-local-lift-require
                      syntax-local-lift-provide
                      syntax-local-lift-module-end-declaration
                      
                      syntax-local-module-defined-identifiers
                      syntax-local-module-required-identifiers
                      
                      syntax-local-get-shadower
                      
                      local-expand
                      local-expand/capture-lifts
                      local-transformer-expand
                      local-transformer-expand/capture-lifts
                      syntax-local-expand-expression
                      
                      internal-definition-context?
                      syntax-local-make-definition-context
                      syntax-local-bind-syntaxes
                      internal-definition-context-binding-identifiers
                      internal-definition-context-introduce
                      internal-definition-context-seal
                      identifier-remove-from-definition-context
                      
                      make-set!-transformer
                      prop:set!-transformer
                      set!-transformer?
                      set!-transformer-procedure

                      rename-transformer?
                      prop:rename-transformer
                      make-rename-transformer
                      rename-transformer-target

                      ;; This list will need to be a lot longer...
                      list cons car cdr pair? null? map filter append
                      equal? values void
                      symbol->string
                      regexp-match?
                      hash-ref
                      assv
                      error
                      println
                      random
                      +)
