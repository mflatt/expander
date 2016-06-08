#lang racket/base
(require "set.rkt"
         "srcloc.rkt"
         "phase.rkt"
         (except-in "scope.rkt"
                    syntax-e
                    bound-identifier=?
                    syntax-shift-phase-level)
         "namespace.rkt"
         (except-in "binding.rkt"
                    free-identifier=?
                    identifier-binding
                    identifier-binding-symbol)
         "core.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "liberal-def-ctx.rkt"
         "syntax-local.rkt"
         "def-ctx.rkt"
         "local-expand.rkt"
         "taint.rkt"
         "checked-syntax.rkt"
         "syntax-error.rkt"
         "module-path.rkt"
         "variable-reference.rkt"
         "allowed-context.rkt")

(provide primitive-ids)

;; Register core primitives:
(define-syntax-rule (add-core-primitives! #:table primitive-ids id ...)
  (begin
    (define primitive-ids (seteq 'id ...))
    (add-core-primitive! 'id id)
    ...))

(add-core-primitives! #:table primitive-ids
                      
                      syntax?
                      syntax-e
                      syntax->datum
                      datum->syntax

                      bound-identifier=?
                      free-identifier=?
                      free-transformer-identifier=?
                      free-template-identifier=?
                      free-label-identifier=?
                      identifier-binding
                      identifier-transformer-binding
                      identifier-template-binding
                      identifier-label-binding
                      identifier-binding-symbol
                      identifier-prune-lexical-context
                      syntax-debug-info
                      syntax-track-origin
                      syntax-shift-phase-level
                      syntax-source-module
                      identifier-prune-to-source-module
                      
                      syntax-source
                      syntax-line
                      syntax-column
                      syntax-position
                      syntax-span
                      syntax->list
                      syntax-property
                      syntax-original?
                      
                      syntax-tainted?
                      syntax-arm
                      syntax-disarm
                      syntax-rearm
                      syntax-taint
                      
                      raise-syntax-error
                      exn:fail:syntax
                      make-exn:fail:syntax
                      exn:fail:syntax?
                      exn:fail:syntax-exprs
                      
                      syntax-transforming?
                      syntax-transforming-with-lifts?
                      syntax-transforming-module-expression?
                      syntax-local-transforming-module-provides?
                      
                      syntax-local-context
                      syntax-local-introduce
                      syntax-local-identifier-as-binding
                      syntax-local-phase-level
                      syntax-local-name

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
                      syntax-local-module-exports
                      syntax-local-submodules
                      
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

                      prop:liberal-define-context
                      liberal-define-context?
                      
                      prop:expansion-contexts

                      resolved-module-path?
                      make-resolved-module-path
                      resolved-module-path-name
                      
                      module-path-index?
                      module-path-index-resolve
                      module-path-index-join
                      module-path-index-split
                      module-path-index-submodule

                      current-module-name-resolver
                      current-module-declare-name
                      
                      current-namespace
                      namespace-module-registry
                      namespace?
                      
                      variable-reference->empty-namespace
                      variable-reference->namespace
                      variable-reference->resolved-module-path
                      variable-reference->module-path-index
                      variable-reference->module-source
                      variable-reference->phase
                      variable-reference->module-base-phase
                      variable-reference->module-declaration-inspector)
