#lang racket/base
(require "serialize.rkt"
         "../host/linklet.rkt"
         "../namespace/core.rkt"
         "../syntax/scope.rkt"
         "../namespace/namespace.rkt"
         "../expand/root-expand-context.rkt"
         "../compile/reserved-symbol.rkt"
         "compiled-in-memory.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "instance.rkt"
         "eager-instance.rkt"
         "expr.rkt"
         "form.rkt"
         "multi-top.rkt"
         "namespace-scope.rkt")

(provide compile-single
         compile-top)

;; Compile a stand-alone expression, such as the right-hand side of a
;; `define-syntaxes` in a module
(define (compile-single s cctx)
  (compile-top s cctx
               #:serializable? #f
               #:single-expression? #t))

;; Compile a single form, which can be a `define-values` form, a
;; `define-synatxes` form, or an expression (where `begin` is treated
;; as an expression form). If `serializable?` is false, don't bother
;; generating the linklet for serialized data, because it won't be
;; used. If `to-source?` is true, the result is a hash table containing
;; S-expression linkets, instead of a `compiled-in-memory` containing
;; compiled linklets.
(define (compile-top s cctx
                     #:serializable? [serializable? #t]
                     #:single-expression? [single-expression? #f]
                     #:to-source? [to-source? #f])
  (define phase (compile-context-phase cctx))

  (define mpis (make-module-path-index-table))

  ;; Compile the body forms, similar to compiling the body of a module
  (define-values (body-linklets
                  min-phase
                  max-phase
                  phase-to-link-module-uses
                  phase-to-link-module-uses-expr
                  phase-to-link-extra-inspectorsss
                  syntax-literalss
                  no-root-context-syntax-literals)
    (compile-forms (list s) cctx mpis
                   #:body-imports (if single-expression?
                                      `([]
                                        [,syntax-literalss-id]
                                        [])
                                      `([,top-level-bind!-id
                                         ,top-level-require!-id]
                                        [,mpi-vector-id
                                         ,syntax-literalss-id]
                                        ,instance-imports))
                   #:to-source? to-source?
                   #:other-form-callback compile-top-level-require))

  (define (add-metadata ht)
    (let* ([ht (hash-set ht 'original-phase phase)]
           [ht (hash-set ht 'max-phase max-phase)])
      ht))
  
  (define bundle
    ((if to-source? values hash->linklet-bundle)
     (add-metadata
      (cond
       [serializable?
        ;; To support seialization, construct a linklet that will
        ;; deserialize module path indexes, syntax objects, etc.
        (define syntax-literalss-expr
          (generate-eager-syntax-literals! 
           syntax-literalss
           mpis
           phase
           (compile-context-self cctx)
           (compile-context-namespace cctx)))

        (define link-linklet
          ((if to-source? values compile-linklet)
           `(linklet
             ;; imports
             (,deserialize-imports
              ,eager-instance-imports)
             ;; exports
             (,mpi-vector-id
              ,deserialized-syntax-vector-id
              phase-to-link-modules
              ,syntax-literalss-id)
             (define-values (,mpi-vector-id)
               ,(generate-module-path-index-deserialize mpis))
             (define-values (,deserialized-syntax-vector-id) 
               (make-vector ,(add1 phase) #f))
             (define-values (phase-to-link-modules) ,phase-to-link-module-uses-expr)
             (define-values (,syntax-literalss-id) ,syntax-literalss-expr))))
        
        (hash-set body-linklets 'link link-linklet)]
       [else
        ;; Will combine the linking unit with non-serialized link info
        body-linklets]))))
  
  (cond
   [to-source?
    (hasheq #f bundle)]
   [else
    ;; If the compiled code is executed directly, it must be in its
    ;; original phase, and we'll share the original values
    (compiled-in-memory (hash->linklet-directory (hasheq #f bundle))
                        phase-to-link-module-uses
                        (current-code-inspector)
                        phase-to-link-extra-inspectorsss
                        (mpis-as-vector mpis)
                        (syntax-literals-as-vectors syntax-literalss phase)
                        null
                        null
                        (extract-namespace-scopes (compile-context-namespace cctx)))]))

;; Callback for compiling a sequence of expressions: handle `require`
;; (which is handled separately for modules)
(define (compile-top-level-require s cctx)
  (define phase (compile-context-phase cctx))
  (case (core-form-sym s phase)
    [(#%require)
     (define form-stx (compile-quote-syntax s phase cctx))
     `(,top-level-require!-id ,form-stx ,ns-id)]
    [else #f]))
