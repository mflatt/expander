#lang racket/base
(require "serialize.rkt"
         "linklet.rkt"
         "compiled-top.rkt"
         "compile-context.rkt"
         "compile-header.rkt"
         "compile-impl-id.rkt"
         "compile-instance.rkt"
         "compile-expr.rkt"
         "compile-form.rkt"
         "compile-module.rkt")

(provide make-compile-context

         compile-single
         compile-top
         compiled-top?
         
         compile-module)

;; Returns a `compiled-top`
(define (compile-single s cctx)
  (compile-top s cctx #:serializable? #f))

;; Returns a `compiled-top`
(define (compile-top s cctx
                     #:serializable? [serializable? #t])
  (define phase (compile-context-phase cctx))

  (define mpis (make-module-path-index-table))
  
  (define-values (body-linklets
                  min-phase
                  max-phase
                  phase-to-link-module-uses
                  phase-to-link-module-uses-expr
                  phase-to-syntax-literals)
    (compile-forms (list s) cctx mpis
                   #:phase-in-body-thunk phase))
  
  (define syntax-literalss
    (for/list ([phase (in-range min-phase (add1 max-phase))])
      (or (hash-ref phase-to-syntax-literals phase #f)
          empty-syntax-literals)))
  
  (define code
    (hash->linklet-directory
     (cond
      [serializable?
       (define syntax-literalss-expr
         (generate-eager-syntax-literals! 
          syntax-literalss
          mpis
          phase
          (compile-context-self cctx)))

       (define link-cu
         (compile-linklet
          `(linklet
            #:import ([deserialize ,@deserialize-imports])
            #:export ([,mpi-vector-id mpi-vector]
                      syntax-literals
                      phase-to-link-modules
                      min-phase
                      max-phase)
            (define-values (,mpi-vector-id)
              ,(generate-module-path-index-deserialize mpis))
            (define-values (deserialized-syntax) 
              (make-vector ,(add1 phase) #f))
            (define-values (original-phase) ,phase)
            (define-values (max-phase) ,max-phase)
            (define-values (phase-to-link-modules) ,phase-to-link-module-uses-expr)
            (define syntax-literalss ,syntax-literalss-expr))))
       
       (hash-set body-linklets #"link" link-cu)]
      [else
       ;; Will combine the linking unit with non-serialized link info
       body-linklets])))
  
  ;; If the compiled code is executed directly in its original phase,
  ;; we'll share the original values
  (compiled-top code
                phase
                max-phase
                phase-to-link-module-uses
                (mpis-as-vector mpis)
                (syntax-literals-as-vectors syntax-literalss phase)))

        ;; FIXME --- doesn't belong here
        #;
        [(#%require)
         (define m (match-syntax s '(#%require req ...)))
         ;; Running the compiled code will trigger expander work ---
         ;; which is strange, and that reflects how a top-level
         ;; `#%require` is strange
         `(,(lambda ()
              (define ns (compile-context-namespace cctx))
              (parse-and-perform-requires! #:run? #t (m 'req) #f ns phase 
                                           (make-requires+provides #f))))]
