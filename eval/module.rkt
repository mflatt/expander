#lang racket/base
(require racket/promise
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/inspector.rkt"
         "../common/phase.rkt"
         "../compile/module-use.rkt"
         "../common/module-path.rkt"
         "../compile/serialize.rkt"
         "../host/linklet.rkt"
         "../compile/instance.rkt"
         "../compile/compiled-in-memory.rkt"
         "../expand/context.rkt"
         "../expand/root-expand-context.rkt"
         "root-context.rkt")

;; Run a representation of top-level code as produced by `compile-module`;
;; see "compile.rkt" and "compile-module.rkt"

(provide eval-module
         compiled-module->declaration-instance)

(define (eval-module c
                     #:namespace [ns (current-namespace)]
                     #:as-submodule? [as-submodule? #f]
                     #:supermodule-name [supermodule-name #f]) ; for submodules declared with module
  (define-values (dh h data-instance declaration-instance)
    (compiled-module->dh+h+data-instance+declaration-instance c))
  
  (define (decl key)
    (instance-variable-value declaration-instance key))
  
  (define (declare-submodules names pre?)
    (when dh
      (if (compiled-in-memory? c)
          (for ([c (in-list (if pre?
                                (compiled-in-memory-pre-compiled-in-memorys c)
                                (compiled-in-memory-post-compiled-in-memorys c)))])
            (eval-module c #:namespace ns #:supermodule-name declare-name))
          (for ([name (in-list names)])
            (define sm-cd (hash-ref dh name #f))
            (unless sm-cd (error "missing submodule declaration:" name))
            (eval-module sm-cd #:namespace ns #:supermodule-name declare-name)))))
  
  (define pre-submodule-names (hash-ref h 'pre null))
  (define post-submodule-names (hash-ref h 'post null))
  (define default-name (hash-ref h 'name 'module))
  
  (define original-self (decl 'self-mpi))

  (define phase-to-link-modules (decl 'phase-to-link-modules))

  (define min-phase (decl 'min-phase))
  (define max-phase (decl 'max-phase))
  (define evaled-h (for*/hash ([phase-level (in-range min-phase (add1 max-phase))]
                               [v (in-value (hash-ref h phase-level #f))]
                               #:when v)
                     (values phase-level (eval-linklet v))))
  (define syntax-literals-linklet (eval-linklet (hash-ref h 'stx)))
  
  (define requires (decl 'requires))
  (define provides (decl 'provides))
  
  (define create-root-expand-context-from-module ; might be used to create root-expand-context
    (make-create-root-expand-context-from-module requires evaled-h))
  
  (define m (make-module original-self
                         requires
                         provides
                         #:language-info (decl 'language-info)
                         min-phase
                         max-phase
                         #:cross-phase-persistent? (decl 'cross-phase-persistent?)
                         #:submodule-names (append pre-submodule-names post-submodule-names)
                         #:supermodule-name supermodule-name
                         (lambda (data-box ns phase-shift phase-level self bulk-binding-registry insp)
                           (define syntax-literals-instance
                             (init-syntax-literals! data-box ns
                                                    syntax-literals-linklet data-instance
                                                    phase-shift original-self self bulk-binding-registry insp
                                                    create-root-expand-context-from-module))
                           (define cu (hash-ref evaled-h phase-level #f))
                           (when cu
                             (define imports
                               (for/list ([mu (in-list (hash-ref phase-to-link-modules phase-level))])
                                 (namespace-module-use->instance ns mu
                                                                 #:shift-from original-self
                                                                 #:shift-to self
                                                                 #:phase-shift
                                                                 (phase+ (phase- phase-level (module-use-phase mu))
                                                                         phase-shift))))
                             (define inst
                               (make-instance-instance
                                #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:bulk-binding-registry bulk-binding-registry
                                #:inspector insp
                                #:set-transformer! (lambda (name val)
                                                     (namespace-set-transformer! ns (sub1 phase-level) name val))))
                             (define (instantiate-body)
                               (instantiate-linklet cu (list* data-instance
                                                              syntax-literals-instance
                                                              inst
                                                              imports)
                                                    (namespace->instance ns phase-level)))
                             (cond
                              [(zero-phase? phase-level)
                               (instantiate-body)]
                              [else
                               ;; For phase level 1 and up, set the expansion context
                               ;; to point back to the module's info:
                               (parameterize ([current-expand-context (delay (make-expand-context ns))]
                                              [current-namespace ns]
                                              [current-module-code-inspector insp])
                                 (instantiate-body))])))))
  
  (define declare-name (substitute-module-declare-name default-name))
  
  (unless as-submodule?
    (declare-submodules pre-submodule-names #t))
 
  (declare-module! ns
                   m
                   declare-name
                   #:as-submodule? as-submodule?)

  (unless as-submodule?
    (declare-submodules post-submodule-names #f)))

;; ----------------------------------------

(define (init-syntax-literals! data-box ns
                               syntax-literals-linklet data-instance
                               phase-shift original-self self bulk-binding-registry insp
                               create-root-expand-context-from-module)
  (unless (unbox data-box)
    (define inst
      (make-instance-instance
       #:namespace ns
       #:phase-shift phase-shift
       #:self self 
       #:bulk-binding-registry bulk-binding-registry
       #:inspector insp
       #:set-transformer! (lambda (name val) (error "shouldn't get here for the root-ctx linklet"))))
    
    (define root-ctx-instance
      (instantiate-linklet syntax-literals-linklet
                           (list deserialize-instance
                                 data-instance
                                 inst)))
    
    (set-box! data-box root-ctx-instance)

    (define get-encoded-root-expand-ctx
      (instance-variable-value root-ctx-instance 'get-encoded-root-expand-ctx))
    
    (cond
     [(eq? get-encoded-root-expand-ctx 'empty)
      ;; A `#:empty-namespace` declaration requested a namespace with no initial bindings
      (namespace-set-root-expand-ctx! ns (delay (make-root-expand-context)))]
     [(procedure? get-encoded-root-expand-ctx)
      ;; Root expand context has been preserved; deserialize it on demand
      (namespace-set-root-expand-ctx! ns (delay (root-expand-context-decode-for-module
                                                 (get-encoded-root-expand-ctx))))]
     [else
      ;; Root expand context has not been preserved, because it can be reconstructed
      ;; from module metadata; do that on demand
      (namespace-set-root-expand-ctx! ns (delay (create-root-expand-context-from-module
                                                 ns phase-shift original-self self)))]))

  (unbox data-box))

;; ----------------------------------------

;; Returns:
;;
;;   dh - hash from linklet directory to access submodules, or #f if
;;   no submodules
;;
;;   h - hash from the module's linklet bundle
;;
;;  data-instance - provides data, either extracted from
;;  compiled-in-memory or instantiated from the bundle
;;
;;  declaration-instance - provides metadata, extracted from the
;;  bundle and linked with `data-instance`
(define (compiled-module->dh+h+data-instance+declaration-instance c)
  (define ld/h (if (compiled-in-memory? c)
                   (compiled-in-memory-linklet-directory c)
                   c))
  (define dh (cond
              [(linklet-directory? ld/h)
               ;; has submodules
               (linklet-directory->hash ld/h)]
              [else
               ;; no submodules
               #f]))
  (define h (linklet-bundle->hash (if dh
                                      (hash-ref dh #f)
                                      ld/h)))

  (define data-instance
    (if (compiled-in-memory? c)
        (make-data-instance-from-compiled-in-memory c)
        (instantiate-linklet (eval-linklet (hash-ref h 'data))
                             (list deserialize-instance))))

  (define declaration-instance
    (instantiate-linklet (eval-linklet (hash-ref h 'decl))
                         (list deserialize-instance
                               data-instance)))
  
  (values dh h data-instance declaration-instance))

(define (compiled-module->declaration-instance c)
  (define-values (dh h data-instance declaration-instance)
    (compiled-module->dh+h+data-instance+declaration-instance c))
  declaration-instance)

;; ----------------------------------------

(define (make-data-instance-from-compiled-in-memory cim)
  (define data-instance (make-instance 'data))
  (instance-set-variable-value! data-instance 'mpi-vector
                                (compiled-in-memory-mpis cim))
  (instance-set-variable-value! data-instance 'deserialized-syntax
                                (compiled-in-memory-syntax-literalss cim))
  data-instance)
