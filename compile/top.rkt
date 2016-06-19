#lang racket/base
(require "serialize.rkt"
         "../host/linklet.rkt"
         "../namespace/core.rkt"
         "../syntax/scope.rkt"
         "../namespace/namespace.rkt"
         "../expand/root-expand-context.rkt"
         "compiled-in-memory.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "instance.rkt"
         "eager-instance.rkt"
         "expr.rkt"
         "form.rkt"
         "multi-top.rkt")

(provide compile-single
         compile-top)

;; Compile a stand-alone expression, such as the right-hand side of a
;; `define-syntaxes` in a module
(define (compile-single s cctx)
  (compile-top s cctx #:serializable? #f))

;; Compile a single form, which can be a `define-values` form, a
;; `define-synatxes` form, or an expression (where `begin` is treated
;; as an expression form). If `serializable?` is false, don't bother
;; generating the linklet for serialized data, because it won't be
;; used.
(define (compile-top s cctx
                     #:serializable? [serializable? #t])
  (define phase (compile-context-phase cctx))

  (define mpis (make-module-path-index-table))
  
  (define-values (body-linklets
                  min-phase
                  max-phase
                  phase-to-link-module-uses
                  phase-to-link-module-uses-expr
                  syntax-literalss
                  no-root-context-syntax-literals)
    (compile-forms (list s) cctx mpis
                   #:phase-in-body-thunk phase
                   #:other-form-callback compile-top-level-require))

  (define bundle
    (hash->linklet-bundle
     (cond
      [serializable?
       (define syntax-literalss-expr
         (generate-eager-syntax-literals! 
          syntax-literalss
          mpis
          phase
          (compile-context-self cctx)
          (compile-context-namespace cctx)))

       (define link-cu
         (compile-linklet
          `(linklet
            #:import ([deserialize ,@deserialize-imports]
                      [eager-instance ,@eager-instance-imports])
            #:export ([,mpi-vector-id mpi-vector]
                      deserialized-syntax
                      original-phase
                      max-phase
                      phase-to-link-modules
                      syntax-literalss)
            (define-values (,mpi-vector-id)
              ,(generate-module-path-index-deserialize mpis))
            (define-values (deserialized-syntax) 
              (make-vector ,(add1 phase) #f))
            (define-values (original-phase) ,phase)
            (define-values (max-phase) ,max-phase)
            (define-values (phase-to-link-modules) ,phase-to-link-module-uses-expr)
            (define-values (syntax-literalss) ,syntax-literalss-expr))))
       
       (hash-set body-linklets 'link link-cu)]
      [else
       ;; Will combine the linking unit with non-serialized link info
       body-linklets])))
  
  ;; If the compiled code is executed directly in its original phase,
  ;; we'll share the original values
  (compiled-in-memory (hash->linklet-directory (hash #f bundle))
                      phase
                      max-phase
                      phase-to-link-module-uses
                      (mpis-as-vector mpis)
                      (syntax-literals-as-vectors syntax-literalss phase)
                      null
                      null))

(define (compile-top-level-require s cctx)
  (define phase (compile-context-phase cctx))
  (case (core-form-sym s phase)
    [(#%require)
     (define form-stx (compile-quote-syntax s phase cctx))
     `(,top-level-require!-id ,form-stx ,ns-id)]
    [else #f]))
