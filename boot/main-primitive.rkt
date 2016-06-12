#lang racket/base
(require "../eval/eval.rkt"
         "../eval/dynamic-require.rkt"
         (prefix-in wrapper: "../eval/main.rkt")
         "../namespace/namespace.rkt"
         "../namespace/eval.rkt"
         "../namespace/attach.rkt")

(provide main-primitives)

(define main-primitives
  (hasheq 'eval wrapper:eval
          'eval-syntax wrapper:eval-syntax
          'compile wrapper:compile
          'compile-syntax wrapper:compile-syntax
          'expand wrapper:expand
          'expand-syntax wrapper:expand-syntax
          'dynamic-require dynamic-require

          'compiled-module-expression? compiled-module-expression?

          'make-empty-namespace make-empty-namespace

          'namespace-attach-module namespace-attach-module
          'namespace-attach-module-declaration namespace-attach-module-declaration

          'namespace-symbol->identifier namespace-symbol->identifier
          'namespace-module-identifier namespace-module-identifier
          'namespace-syntax-introduce namespace-syntax-introduce
          'namespace-require namespace-require
          'namespace-require/copy namespace-require/copy
          'namespace-require/constant namespace-require/constant
          'namespace-require/expansion-time namespace-require/expansion-time
          'namespace-variable-value namespace-variable-value
          'namespace-set-variable-value! namespace-set-variable-value!
          'namespace-undefine-variable!	namespace-undefine-variable!
          'namespace-mapped-symbols namespace-mapped-symbols 
          'namespace-base-phase namespace-base-phase          
          
          'module-declared? module-declared?))

