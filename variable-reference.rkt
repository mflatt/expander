#lang racket/base
(require "namespace.rkt"
         "contract.rkt")

(provide variable-reference
         
         variable-reference?
         variable-reference-constant?
         variable-reference->empty-namespace
         variable-reference->namespace
         variable-reference->resolved-module-path
         variable-reference->module-path-index
         variable-reference->module-source
         variable-reference->phase
         variable-reference->module-base-phase
         variable-reference->module-declaration-inspector)

(struct variable-reference (mpi namespace phase base-phase constant?))

(define (variable-reference->empty-namespace vr)
  (check 'variable-reference->empty-namespace variable-reference? vr)
  (make-empty-namespace (variable-reference-namespace vr)))

(define (variable-reference->namespace vr)
  (check 'variable-reference->namespace variable-reference? vr)
  (variable-reference-namespace vr))

(define (variable-reference->resolved-module-path vr)
  (check 'variable-reference->resolved-module-path variable-reference? vr)
  (namespace-module-name (variable-reference-namespace vr)))

(define (variable-reference->module-path-index vr)
  (check 'variable-reference->module-path-index variable-reference? vr)
  (variable-reference-mpi vr))

(define (variable-reference->module-source vr)
  (check 'variable-reference->module-source variable-reference? vr)
  (variable-reference->resolved-module-path vr))

(define (variable-reference->phase vr)
 (check 'variable-reference->phase variable-reference? vr)
 (variable-reference->phase vr))

(define (variable-reference->module-base-phase vr)
  (check 'variable-reference->module-base-phase variable-reference? vr)
  (variable-reference-base-phase vr))

(define (variable-reference->module-declaration-inspector vr)
  (check 'variable-reference->base-phase variable-reference? vr)
  ;; FIXME
  (current-code-inspector))
