#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "phase.rkt"
         "core.rkt"
         "module-path.rkt"
         "module-use.rkt"
         "serialize.rkt"
         "side-effect.rkt"
         "built-in-symbol.rkt"
         "linklet.rkt"
         "compile-context.rkt"
         "compile-header.rkt"
         "compile-impl-id.rkt"
         "compile-def-id.rkt"
         "compile-instance.rkt"
         "compile-form.rkt"
         "compiled-in-memory.rkt")

(provide compile-module)

;; Compiles module to a set of linklets that is returned as a
;; `compiled-in-memory`
(define (compile-module s cctx
                        #:self [given-self #f]
                        #:as-submodule? [as-submodule? #f])
  (define m (match-syntax s '(module name initial-require
                              (#%module-begin body ...))))
  (define enclosing-self (compile-context-module-self cctx))
  (define self (or given-self
                   (make-generic-self-module-path-index
                    (make-self-module-path-index
                     (syntax-e (m 'name))
                     enclosing-self))))
  (define root-module-name (or (compile-context-root-module-name cctx)
                               (syntax-e (m 'name))))
  (define requires (syntax-property s 'module-requires))
  (define provides (syntax-property s 'module-provides))
  (define bodys (m 'body))

  (define mpis (make-module-path-index-table))
  
  (define body-cctx (struct-copy compile-context cctx
                                 [phase 0]
                                 [self self]
                                 [module-self self]
                                 [root-module-name root-module-name]
                                 [lazy-syntax-literals? #t]))
  
  (define cross-phase-persistent? #f)
  
  ;; Callback to track phases that have side effects
  (define side-effects (make-hasheqv))
  (define (check-side-effects! e ; compiled expression
                               expected-results ; number of expected reuslts, or #f if any number is ok
                               phase)
    (when (any-side-effects? e expected-results)
      (hash-set! side-effects phase #t)))

  ;; Compile the sequence of body forms:
  (define-values (body-linklets
                  min-phase
                  max-phase
                  phase-to-link-module-uses
                  phase-to-link-module-uses-expr
                  syntax-literalss)
    (compile-forms bodys body-cctx mpis
                   #:compiled-expression-callback check-side-effects!
                   #:other-form-callback (lambda (body phase)
                                           (case (core-form-sym body phase)
                                             [(#%declare)
                                              (define m (match-syntax body '(#%declare kw ...)))
                                              (for ([kw (in-list (m 'kw))])
                                                (when (eq? (syntax-e kw) '#:cross-phase-persistent)
                                                  (set! cross-phase-persistent? #t)))]))))

  ;; Compile submodules; each list is (cons linklet-directory-key compiled-in-memory)
  (define pre-submodules (compile-submodules 'module
                                             #:bodys bodys
                                             #:as-submodule? as-submodule?
                                             #:cctx body-cctx))
  (define post-submodules (compile-submodules 'module*
                                              #:bodys bodys
                                              #:as-submodule? as-submodule?
                                              #:cctx body-cctx))
  
  (define (get-submodule-linklet-directory p)
    (compiled-in-memory-linklet-directory (cdr p)))

  ;; Generate module-declaration info, which includes linking
  ;; information for each phase
  (define declaration-body
    `((define-values (self-mpi) ,(add-module-path-index! mpis self))
      (define-values (default-name) ',(resolved-module-path-name
                                       (module-path-index-resolve self)))
      (define-values (root-module-name) ',root-module-name)
      (define-values (cross-phase-persistent?) ,cross-phase-persistent?)
      (define-values (requires) ,(generate-deserialize requires mpis))
      (define-values (provides) ,(generate-deserialize provides mpis))
      (define-values (side-effects) ',(sort (hash-keys side-effects) <))
      (define-values (min-phase) ,min-phase)
      (define-values (max-phase) ,max-phase)
      (define-values (phase-to-link-modules) ,phase-to-link-module-uses-expr)
      (define-values (pre-submodules) ',(map car pre-submodules))
      (define-values (post-submodules) ',(map car post-submodules))))
  
  ;; Assemble the declaration linking unit, which is instanted
  ;; once for a module declaration and shared among instances
  (define declaration-linklet
    (compile-linklet
     `(linklet
       #:import ([deserialize ,@deserialize-imports]
                 [data (mpi-vector ,mpi-vector-id)
                       deserialized-syntax])
       #:export (self-mpi
                 default-name
                 root-module-name
                 requires
                 provides
                 variables
                 side-effects
                 cross-phase-persistent?
                 min-phase
                 max-phase
                 phase-to-link-modules
                 pre-submodules
                 post-submodules)
       ,@declaration-body)))
  
  ;; The data linklet houses serialized data for use by the
  ;; declaration and module-body linklets. In the case of syntax
  ;; objects, it provides a shared vector for all instances, while
  ;; the unmarshaling of data is left to each body.
  (define data-linklet
    (compile-linklet
     `(linklet
       #:import ([deserialize ,@deserialize-imports])
       #:export ([,mpi-vector-id mpi-vector]
                 deserialized-syntax)
       (define-values (,mpi-vector-id)
         ,(generate-module-path-index-deserialize mpis))
       (define-values (deserialized-syntax)
         (make-vector ,(add1 max-phase) #f)))))

  ;; Combine everything in a linklet directosy
  (define ld
    (hash->linklet-directory
     (for/fold ([ht (hash-set (hash-set body-linklets #".decl" declaration-linklet)
                              #".data"
                              data-linklet)])
               ([sm (in-list (append pre-submodules post-submodules))])
       (hash-set ht
                 (encode-linklet-directory-key (car sm))
                 (compiled-in-memory-linklet-directory (cdr sm))))))
 
  ;; Save mpis and syntax for direct evaluation, instead of unmarshaling:
  (compiled-in-memory ld
                      0
                      max-phase
                      phase-to-link-module-uses
                      (mpis-as-vector mpis)
                      (syntax-literals-as-vectors syntax-literalss 0)
                      (map cdr pre-submodules)
                      (map cdr post-submodules)))

;; ----------------------------------------

;; Walk though body to extract and compile submodules that are
;; declared with `form-name` (which is 'module or 'module*)
(define (compile-submodules form-name
                            #:bodys bodys
                            #:as-submodule? as-submodule?
                            #:cctx body-cctx)
  (cond
   [as-submodule?
    null]
   [else
    (let loop ([bodys bodys]
               [phase 0])
      (cond
       [(null? bodys) null]
       [else
        (define f (core-form-sym (car bodys) phase))
        (cond
         [(eq? f form-name)
          (define sm-m (match-syntax (car bodys) '(_ name . _)))
          (define s-shifted
            (cond
             [(try-match-syntax (car bodys) '(module* name #f . _))
              (syntax-shift-phase-level (car bodys) (phase- 0 phase))]
             [else (car bodys)]))
          (cons (cons (syntax-e (sm-m 'name))
                      (compile-module s-shifted body-cctx))
                (loop (cdr bodys) phase))]
         [(eq? f 'begin-for-syntax)
          (define m (match-syntax (car bodys) `(begin-for-syntax e ...)))
          (append (loop (m 'e) (add1 phase))
                  (loop (cdr bodys) phase))]
         [else
          (loop (cdr bodys) phase)])]))]))
