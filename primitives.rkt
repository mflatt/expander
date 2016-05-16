#lang racket/base
(require racket/set
         (except-in "syntax.rkt"
                    syntax->datum
                    datum->syntax)
         "srcloc.rkt"
         "phase.rkt"
         (except-in "scope.rkt"
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
         "module-path.rkt"
         "variable-reference.rkt")

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
                      identifier-binding
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
                      
                      syntax-tainted?
                      syntax-arm
                      syntax-disarm
                      syntax-rearm
                      syntax-taint
                      
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
                      
                      variable-reference?
                      variable-reference-constant?
                      variable-reference->empty-namespace
                      variable-reference->namespace
                      variable-reference->resolved-module-path
                      variable-reference->module-path-index
                      variable-reference->module-source
                      variable-reference->phase
                      variable-reference->module-base-phase
                      variable-reference->module-declaration-inspector
                      
                      ;; Needed to recognize cross-phase persistence
                      cons list make-struct-type make-struct-type-property gensym string->uninterned-symbol

                      ;; Things that we don't have to replace, but
                      ;; used in "demo.rkt":
                      car cdr pair? null? map filter append
                      equal? values void
                      symbol->string
                      regexp-match?
                      hash-ref
                      assv
                      error
                      println
                      random
                      +)
