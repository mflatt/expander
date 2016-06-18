#lang racket/base
(require "common/set.rkt"
         "namespace/namespace.rkt"
         "eval/eval.rkt"
         "eval/dynamic-require.rkt"
         "namespace/eval.rkt"
         (prefix-in wrapper: "eval/main.rkt")
         "namespace/attach.rkt"
         "namespace/module-reflect.rkt"
         "namespace/core.rkt"
         "boot/kernel.rkt"
         "boot/main-primitive.rkt"
         "boot/utils-primitive.rkt"
         "boot/place-primitive.rkt"
         "boot/runtime-primitive.rkt"
         "boot/handler.rkt"
         "syntax/checked-syntax.rkt")

(provide boot ; installs handlers: eval, module name resolver, etc.

         ;; This functions are provided for basic testing
         ;; (such as "demo.rkt")
         syntax? syntax-e
         identifier?
         datum->syntax syntax->datum
         syntax-property
         identifier-binding
         syntax-debug-info
         
         syntax-shift-phase-level
         bound-identifier=?
         
         make-namespace
         make-empty-kernel-namespace
         current-namespace
         
         namespace-syntax-introduce
         namespace-require
         dynamic-require
         namespace-module-identifier
         namespace-attach-module
         namespace-attach-module-declaration
         module-declared?
         
         ;; These are direct functions, not ones that use handlers:
         expand
         compile
         eval)

;; ----------------------------------------

;; Register core forms:
(require "expand/expr.rkt"
         "expand/module.rkt"
         "expand/top-level.rkt")

;; Register core primitives:
(require "boot/core-primitive.rkt")

;; ----------------------------------------

(define (make-empty-kernel-namespace)
  (define ns (make-namespace))
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
    (copy-racket-module! name
                         #:namespace ns
                         #:protected? (eq? name '#%foreign)))
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
(dynamic-require ''#%kernel 0)
