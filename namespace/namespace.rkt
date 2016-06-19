#lang racket/base
(require racket/promise
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/module-path.rkt"
         "../expand/root-expand-context.rkt"
         "../host/linklet.rkt")

(provide make-namespace
         namespace?
         current-namespace
         namespace-module-registry
         namespace-phase
         namespace-0-phase
         namespace-get-root-expand-ctx
         namespace-set-root-expand-ctx!
         namespace->namespace-at-phase
         namespace->module
         namespace-mpi
         namespace-bulk-binding-registry
         
         namespace-set-variable!
         namespace-unset-variable!
         namespace-set-transformer!
         namespace-get-variable
         namespace-get-transformer
         
         namespace-declaration-inspector
         namespace-inspector
         set-namespace-inspector!
         
         namespace->instance
         namespace-same-instance?)

(module+ for-module
  (provide (struct-out namespace)
           (struct-out module-registry)
           (struct-out definitions)
           namespace->definitions))

(struct namespace (mpi                 ; module path index (that's already resolved); instance-specific for a module
                   root-expand-ctx     ; delay of box of context for top-level expansion; set by module instantiation
                   phase               ; phase (not phase level!) of this namespace
                   0-phase             ; phase of module instance's phase-level 0
                   phase-to-namespace  ; phase -> namespace for same module  [shared for the same module instance]
                   phase-level-to-definitions ; phase-level -> definitions [shared for the same module instance]
                   module-registry     ; module-registry of (resolved-module-path -> module) [shared among modules]
                   bulk-binding-registry ; (resolved-module-path -> bulk-provide) for resolving bulk bindings on unmarshal
                   submodule-declarations ; resolved-module-path -> module [shared during a module compilation]
                   cross-phase-persistent-namespace ; #f or namespace for persistent instances [shared among modules]
                   declaration-inspector ; declaration-time inspector
                   [inspector #:mutable] ; instantiation-time inspector
                   available-module-instances  ; phase -> list of module-instance [shared among modules]
                   module-instances)   ; instance-key -> module-instance [shared among modules]
        ;;                             ;   where an instance-key is either (cons resolved-module-path 0-phase)
        ;;                             ;   or just a resolved-module-path, the latter for cross phase persistent modules
        #:property prop:custom-write
        (lambda (ns port mode)
          (write-string "#<namespace" port)
          (define n (namespace-mpi ns))
          (unless (top-level-module-path-index? n)
            (fprintf port ":~a" (format-resolved-module-path-name
                                 (resolved-module-path-name
                                  (module-path-index-resolve n)))))
          (define phase (namespace-phase ns))
          (unless (zero-phase? phase)
            (fprintf port ":~s" phase))
          (write-string ">" port)))

;; Wrapper to make the registry opqaue
(struct module-registry (declarations))

(struct definitions (variables      ; linklet instance
                     transformers)) ; sym -> val

(define (make-namespace [share-from-ns #f]
                        #:root-expand-ctx [root-expand-ctx (make-root-expand-context)]
                        #:register? [register? #t])
  (define phase (if share-from-ns
                    (namespace-phase share-from-ns)
                    0))
  (define ns
    (namespace top-level-module-path-index
               (box root-expand-ctx)
               phase
               phase
               (make-hasheqv)    ; phase-to-namespace
               (make-hasheqv)    ; phase-level-to-definitions
               (if share-from-ns
                   (namespace-module-registry share-from-ns)
                   (module-registry (make-hasheq)))
               (if share-from-ns
                   (namespace-bulk-binding-registry share-from-ns)
                   (make-bulk-binding-registry))
               (make-hasheq)     ; submodule-declarations
               (and share-from-ns
                    (or (namespace-cross-phase-persistent-namespace share-from-ns)
                        share-from-ns))
               (current-code-inspector)
               (make-inspector (current-code-inspector))
               (if share-from-ns
                   (namespace-available-module-instances share-from-ns)
                   (make-hasheqv))
               (if share-from-ns
                   (namespace-module-instances share-from-ns)
                   (make-hash))))
  (when register?
    (hash-set! (namespace-phase-to-namespace ns) phase ns))
  ns)

(define current-namespace (make-parameter (make-namespace)))

(define (namespace-get-root-expand-ctx ns)
  (force (unbox (namespace-root-expand-ctx ns))))

(define (namespace-set-root-expand-ctx! ns root-ctx)
  (set-box! (namespace-root-expand-ctx ns) root-ctx))

(define (namespace->module ns name)
  (or (hash-ref (namespace-submodule-declarations ns) name #f)
      (hash-ref (module-registry-declarations (namespace-module-registry ns)) name #f)))

(define (namespace->namespace-at-phase ns phase)
  (or (hash-ref (namespace-phase-to-namespace ns) phase #f)
      (let ([p-ns (struct-copy namespace ns
                               [phase phase])])
        (hash-set! (namespace-phase-to-namespace ns) phase p-ns)
        p-ns)))

(define (namespace->definitions ns phase-level)
  (define d (hash-ref (namespace-phase-level-to-definitions ns) phase-level #f))
  (or d
      (let ()
        (define p-ns (namespace->namespace-at-phase ns (phase+ (namespace-0-phase ns)
                                                               phase-level)))
        (define d (definitions (make-instance p-ns) (make-hasheq)))
        (hash-set! (namespace-phase-level-to-definitions ns) phase-level d)
        d)))

(define (namespace-set-variable! ns phase-level name val)
  (define d (namespace->definitions ns phase-level))
  (instance-set-variable-value! (definitions-variables d) name val))

(define (namespace-unset-variable! ns phase-level name)
  (define d (namespace->definitions ns phase-level))
  (instance-unset-variable! (definitions-variables d) name))

(define (namespace-set-transformer! ns phase-level name val)
  (define d (namespace->definitions ns (add1 phase-level)))
  (hash-set! (definitions-transformers d) name val))

(define (namespace-get-variable ns phase-level name fail-k)
  (define d (namespace->definitions ns phase-level))
  (instance-variable-value (definitions-variables d) name fail-k))
  
(define (namespace-get-transformer ns phase-level name fail-k)
  (define d (namespace->definitions ns (add1 phase-level)))
  (hash-ref (definitions-transformers d) name fail-k))

(define (namespace->instance ns phase-shift)
  (definitions-variables (namespace->definitions ns phase-shift)))

(define (namespace-same-instance? a-ns b-ns)
  (eq? (hash-ref (namespace-phase-level-to-definitions a-ns)
                 (namespace-0-phase a-ns)
                 'no-a)
       (hash-ref (namespace-phase-level-to-definitions b-ns)
                 (namespace-0-phase b-ns)
                 'no-b)))
