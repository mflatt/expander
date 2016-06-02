#lang racket/base
(require "serialize.rkt"
         "linklet.rkt"
         "compiled-top.rkt"
         "compile-context.rkt"
         "compile-header.rkt"
         "compile-impl-id.rkt"
         "compile-instance.rkt"
         "compile-expr.rkt"
         "compile-module.rkt")

(provide make-compile-context

         compile-top
         compiled-top?
         
         compile-module)

;; Returns a linking directory with two linking units
(define (compile-top s cctx
                     #:serializable? [serializable? #t])
  (define mpis (make-module-path-index-table))
  (define header (make-header mpis))
  (define phase (compile-context-phase cctx))
  (define compiled
    (compile s (struct-copy compile-context cctx
                            [header header])))
  (define-values (link-module-uses imports def-decls)
    (generate-links+imports header phase cctx))
  
  (define cu
    (compile-linklet
     `(linklet
       #:import ([instance ,@instance-imports]
                 [link (mpi-vector ,mpi-vector-id)
                       (syntax-literals ,syntax-literals-id)
                       (get-syntax-literal! ,get-syntax-literal!-id)]
                 ,@imports)
       #:export (,@def-decls
                 [,body-thunk-id body-thunk])
       (define-values (,body-thunk-id) (lambda () ,compiled)))))
  
  (define code
    (cond
     [serializable?
      (define syntax-literals
        (generate-syntax-literals! (header-syntax-literals header)
                                   mpis
                                   phase
                                   (compile-context-self cctx)))
      (define link-module-use-exprs
        (serialize-module-uses link-module-uses mpis))
      
      (define link-cu
        (compile-linklet
         `(linklet
           #:import ([deserialize ,@deserialize-imports])
           #:export ([,mpi-vector-id mpi-vector]
                     deserialized-syntax
                     link-modules
                     original-phase)
           (define-values (,mpi-vector-id)
             ,(generate-module-path-index-deserialize mpis))
           (define-values (deserialized-syntax) 
             (make-vector ,(add1 phase) #f))
           (define-values (original-phase) ,phase)
           (define-values (link-modules)
             (list ,@link-module-use-exprs))
           ,@syntax-literals)))
      
      (hash->linklet-directory
       (hash #"top" cu
             #"link" link-cu))]
     [else
      ;; Will combine the linking unit with non-serialized link info
      (hash->linklet-directory
       (hash #"top" cu))]))
  
  ;; If the compiled code is executed directly in its original phase,
  ;; we'll share the original values
  (compiled-top code
                phase
                link-module-uses
                (mpis-as-vector-getter mpis)
                (syntax-literals-as-vector-getter (header-syntax-literals header))))

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
