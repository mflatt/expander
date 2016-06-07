#lang racket/base
(require "set.rkt"
         "namespace.rkt"
         "eval.rkt"
         "namespace-eval.rkt"
         (prefix-in wrapper: "eval-wrapper.rkt")
         "namespace-attach.rkt"
         "core.rkt"
         "kernel.rkt"
         "utils-primitives.rkt"
         "place-primitives.rkt"
         "runtime-primitives.rkt"
         "boot.rkt"
         "checked-syntax.rkt")

(provide boot ; installs handlers: eval, module name resolver, etc.

         ;; This functions are provided for basic testing
         ;; (such as "demo.rkt")
         syntax? syntax-e
         identifier?
         datum->syntax syntax->datum
         syntax-property
         
         syntax-shift-phase-level
         bound-identifier=?
         
         make-empty-namespace
         make-empty-kernel-namespace
         current-namespace
         
         namespace-syntax-introduce
         namespace-require
         dynamic-require
         namespace-module-identifier
         namespace-attach-module
         namespace-attach-module-declaration
         
         ;; These are direct functions, not ones that use handlers:
         expand
         compile
         eval)

;; ----------------------------------------

;; Register core forms:
(require "expand-expr.rkt"
         "expand-module.rkt"
         "expand-top-level.rkt")

;; Register core primitives:
(require "core-primitives.rkt")

;; ----------------------------------------

(define main-primitives
  (hasheq 'eval wrapper:eval
          'eval-syntax wrapper:eval-syntax
          'compile wrapper:compile
          'compile-syntax wrapper:compile-syntax
          'expand wrapper:expand
          'expand-syntax wrapper:expand-syntax
          'dynamic-require dynamic-require

          'make-empty-namespace make-empty-namespace

          'namespace-module-identifier namespace-module-identifier
          'namespace-attach-module namespace-attach-module
          'namespace-attach-module-declaration namespace-attach-module-declaration
          
          'namespace-syntax-introduce namespace-syntax-introduce
          'namespace-require namespace-require
          'namespace-variable-value namespace-variable-value
          'namespace-set-variable-value! namespace-set-variable-value!
          'namespace-undefine-variable!	namespace-undefine-variable!))

(define (make-empty-kernel-namespace)
  (define ns (make-empty-namespace))
  (declare-core-module! ns)
  (declare-hash-based-module! '#%main main-primitives #:namespace ns)
  (declare-hash-based-module! '#%utils utils-primitives #:namespace ns)
  (declare-hash-based-module! '#%place-struct place-struct-primitives #:namespace ns)
  (declare-hash-based-module! '#%boot boot-primitives #:namespace ns)
  (declare-kernel-module! ns
                          #:eval eval
                          #:main-ids (for/set ([name (in-hash-keys main-primitives)])
                                       name))
  (for ([name (in-list runtime-instances)]
        #:unless (eq? name '#%kernel))
    (copy-racket-module! name #:namespace ns))
  (declare-reexporting-module! '#%builtin (list* '#%place-struct
                                                 '#%utils
                                                 '#%boot
                                                 runtime-instances)
                               #:namespace ns
                               #:reexport? #f)
  ns)

;; ----------------------------------------
;; Initial namespace

(current-namespace (make-empty-kernel-namespace))
(namespace-require ''#%kernel (current-namespace))
