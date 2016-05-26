#lang racket/base
(require "set.rkt"
         "syntax.rkt"
         "scope.rkt"
         "binding.rkt"
         "module-path.rkt")

(provide runtime-stx
         runtime-module-name

         runtime-instances)

;; Runtime primitives are implemented in the runtime system (and not
;; shadowed by the expander's primitives). They're re-exported by
;; '#%kernel, but originally exported by a '#%runtime module. The
;; expander needs to generate references to some '#%runtime` bindings.

(define runtime-scope (new-multi-scope))
(define runtime-stx (add-scope empty-syntax runtime-scope))

(define runtime-module-name (make-resolved-module-path '#%runtime))
(define runtime-mpi (module-path-index-join ''#%runtime #f))

(define (add-runtime-primitive! sym)
  (add-binding! (datum->syntax runtime-stx sym)
                (make-module-binding runtime-mpi 0 sym)
                0))

;; This is only a subset that we need to have bound;
;; the rest are added in "kernel.rkt"
(add-runtime-primitive! 'values)
(add-runtime-primitive! 'cons)
(add-runtime-primitive! 'list)
(add-runtime-primitive! 'make-struct-type)
(add-runtime-primitive! 'make-struct-type-property)
(add-runtime-primitive! 'gensym)
(add-runtime-primitive! 'string->uninterned-symbol)

;; Instances that are built into the runtime system:
(define runtime-instances
  '(#%kernel
    #%paramz
    #%expobs
    #%foreign
    #%unsafe
    #%flfxnum
    #%extfl
    #%network
    #%place
    #%futures))

