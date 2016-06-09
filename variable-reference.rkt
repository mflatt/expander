#lang racket/base
(require "namespace.rkt"
         "contract.rkt"
         "module-path.rkt"
         "linklet.rkt")

(provide variable-reference?          ; provided by linklet layer, along with `#%variable-reference`
         variable-reference-constant? ; provided by linklet layer

         variable-reference->empty-namespace
         variable-reference->namespace
         variable-reference->module-path-index
         variable-reference->resolved-module-path
         variable-reference->module-source
         variable-reference->phase
         variable-reference->module-base-phase
         variable-reference->module-declaration-inspector)

(define (variable-reference->empty-namespace vr)
  (check 'variable-reference->empty-namespace variable-reference? vr)
  (make-namespace (variable-reference->namespace vr)))

(define (variable-reference->namespace vr)
  (check 'variable-reference->namespace variable-reference? vr)
  (instance-name (variable-reference->instance vr)))

(define (variable-reference->module-path-index vr)
  (check 'variable-reference->module-path-index variable-reference? vr)
  (define mpi (namespace-mpi (variable-reference->namespace vr)))
  (if (top-level-module-path-index? mpi)
      #f
      mpi))

(define (variable-reference->resolved-module-path vr)
  (check 'variable-reference->resolved-module-path variable-reference? vr)
  (define mpi (variable-reference->module-path-index vr))
  (and mpi (module-path-index-resolve mpi)))

(define (variable-reference->module-source vr)
  (check 'variable-reference->module-source variable-reference? vr)
  (define r (variable-reference->resolved-module-path vr))
  (and r (resolved-module-path-root-name r)))

(define (variable-reference->phase vr)
 (check 'variable-reference->phase variable-reference? vr)
 (namespace-phase (variable-reference->namespace vr)))

(define (variable-reference->module-base-phase vr)
  (check 'variable-reference->module-base-phase variable-reference? vr)
  (namespace-0-phase (variable-reference->namespace vr)))

(define (variable-reference->module-declaration-inspector vr)
  (check 'variable-reference->base-phase variable-reference? vr)
  ;; FIXME
  (current-code-inspector))
