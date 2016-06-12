#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../common/phase.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "module-use.rkt"
         "serialize.rkt"
         "side-effect.rkt"
         "built-in-symbol.rkt"
         "../host/linklet.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "id-lto-symbol.rkt"
         "instance.rkt"
         "form.rkt"
         "compiled-in-memory.rkt"
         "../host/correlate.rkt")

(provide compile-module)

;; Compiles module to a set of linklets that is returned as a
;; `compiled-in-memory`
(define (compile-module s cctx
                        #:self [given-self #f]
                        #:as-submodule? [as-submodule? #f]
                        #:serializable? [serializable? (not as-submodule?)])
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
  (define encoded-root-expand-ctx (syntax-property s 'root-expand-context)) ; for `module->namespace`
  (define language-info (filter-language-info (syntax-property s 'module-language)))
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
    (unless (hash-ref side-effects phase #f)
      (when (any-side-effects? e expected-results)
        (hash-set! side-effects phase #t))))

  ;; Compile the sequence of body forms:
  (define-values (body-linklets
                  min-phase
                  max-phase
                  phase-to-link-module-uses
                  phase-to-link-module-uses-expr
                  syntax-literalss
                  root-ctx-syntax-literals)
    (compile-forms bodys body-cctx mpis
                   #:encoded-root-expand-ctx encoded-root-expand-ctx
                   #:compiled-expression-callback check-side-effects!
                   #:other-form-callback (lambda (body cctx)
                                           (case (core-form-sym body (compile-context-phase cctx))
                                             [(#%declare)
                                              (define m (match-syntax body '(#%declare kw ...)))
                                              (for ([kw (in-list (m 'kw))])
                                                (when (eq? (syntax-e kw) '#:cross-phase-persistent)
                                                  (set! cross-phase-persistent? #t)))
                                              #f]
                                             [else #f]))))
  
  (define all-syntax-literalss
    (if root-ctx-syntax-literals
        (append syntax-literalss (list root-ctx-syntax-literals))
        syntax-literalss))
  
  ;; Compile submodules; each list is (cons linklet-directory-key compiled-in-memory)
  (define pre-submodules (compile-submodules 'module
                                             #:bodys bodys
                                             #:as-submodule? as-submodule?
                                             #:serializable? serializable?
                                             #:cctx body-cctx))
  (define post-submodules (compile-submodules 'module*
                                              #:bodys bodys
                                              #:as-submodule? as-submodule?
                                              #:serializable? serializable?
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
      (define-values (post-submodules) ',(map car post-submodules))
      (define-values (language-info) ',language-info)))

  ;; Assemble the declaration linking unit, which is instanted
  ;; once for a module declaration and shared among instances
  (define declaration-linklet
    (compile-linklet
     `(linklet
       #:import ([deserialize ,@deserialize-imports]
                 [data (mpi-vector ,mpi-vector-id)])
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
                 post-submodules
                 language-info)
       ,@declaration-body)))
  
  ;; Assemble a linklet that deserializes syntax objects on demand.
  ;; Include an encoding of the root expand context, if any, so that
  ;; `module->namespace` can have the same scopes as literal syntax
  ;; objects in the module.
  (define syntax-literals-linklet
    (compile-linklet
     `(linklet
       #:import ([deserialize ,@deserialize-imports]
                 [data (mpi-vector ,mpi-vector-id)
                       (deserialized-syntax ,deserialized-syntax-id)]
                 [instance ,@instance-imports])
       #:export ([,syntax-literalss-id syntax-literalss]
                 [,get-syntax-literal!-id get-syntax-literal!]
                 ,@(if root-ctx-syntax-literals
                       `(get-encoded-root-expand-ctx)
                       null))
       ,@(generate-lazy-syntax-literals! all-syntax-literalss mpis self
                                         #:skip-deserialize? (not serializable?))
       ,@(if root-ctx-syntax-literals
             `((define-values (get-encoded-root-expand-ctx)
                 (lambda ()
                   ,(generate-lazy-syntax-literal-lookup (add1 max-phase) 0))))
             null))))

  ;; The data linklet houses deserialized data for use by the
  ;; declaration and module-body linklets. In the case of syntax
  ;; objects, it provides a shared vector for all instances, while
  ;; the unmarshaling of data is left to each body.
  (define data-linklet
    (and serializable?
         (compile-linklet
          `(linklet
            #:import ([deserialize ,@deserialize-imports])
            #:export ([,mpi-vector-id mpi-vector]
                      deserialized-syntax)
            (define-values (,mpi-vector-id)
              ,(generate-module-path-index-deserialize mpis))
            (define-values (deserialized-syntax)
              (make-vector ,(+ 2 max-phase) #f))))))
  
  (define combined-linklets
    (let* ([linklets (hash-set body-linklets #".decl" declaration-linklet)]
           [linklets (if data-linklet
                         (hash-set linklets #".data" data-linklet)
                         linklets)]
           [linklets (hash-set linklets #".stx" syntax-literals-linklet)])
      linklets))

  ;; Combine with submodules in a linklet directory
  (define ld
    (hash->linklet-directory
     (for/fold ([ht combined-linklets])
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
                      (syntax-literals-as-vectors all-syntax-literalss 0)
                      (map cdr pre-submodules)
                      (map cdr post-submodules)))

;; ----------------------------------------

;; Walk though body to extract and compile submodules that are
;; declared with `form-name` (which is 'module or 'module*)
(define (compile-submodules form-name
                            #:bodys bodys
                            #:as-submodule? as-submodule?
                            #:serializable? serializable?
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
                      (compile-module s-shifted body-cctx
                                      #:serializable? serializable?))
                (loop (cdr bodys) phase))]
         [(eq? f 'begin-for-syntax)
          (define m (match-syntax (car bodys) `(begin-for-syntax e ...)))
          (append (loop (m 'e) (add1 phase))
                  (loop (cdr bodys) phase))]
         [else
          (loop (cdr bodys) phase)])]))]))

;; ----------------------------------------

(define (filter-language-info li)
  (and (vector? li)
       (= 3 (vector-length li))
       (module-path? (vector-ref li 0))
       (symbol? (vector-ref li 1))
       li))
