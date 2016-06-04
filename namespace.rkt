#lang racket/base
(require "phase.rkt"
         "scope.rkt"
         "bulk-binding.rkt"
         "module-path.rkt"
         "module-use.rkt"
         "root-expand-context.rkt"
         "linklet.rkt"
         "built-in-symbol.rkt")

(provide make-empty-namespace
         namespace?
         current-namespace
         namespace-module-registry
         namespace-phase
         namespace-0-phase
         namespace-root-expand-ctx
         namespace->namespace-at-phase
         make-module-namespace
         namespace->module
         namespace-mpi
         namespace-bulk-binding-registry
         raise-unknown-module-error
         
         make-module
         declare-module!
         module-self
         module-requires
         module-provides
         module-primitive?
         module-cross-phase-persistent?

         namespace-module-instantiate!
         namespace-module-visit!
         namespace->module-namespace
         namespace-same-instance?
         
         namespace-set-variable!
         namespace-set-transformer!
         namespace-get-variable
         namespace-get-transformer
         
         namespace->instance
         namespace-module-use->instance)

(struct namespace (mpi                 ; module path index (that's already resolved)
                   root-expand-ctx     ; module context for top-level expansion
                   phase               ; phase (not phase level! not base phase!) of this namespace
                   0-phase             ; phase of module's phase-level 0
                   phase-to-namespace  ; phase -> namespace for same module  [shared for the same module instance]
                   phase-level-to-definitions ; phase-level -> definitions [shared for the same module instance]
                   module-registry     ; module-registry of (resolved-module-path -> module) [shared among modules]
                   bulk-binding-registry ; (resolved-module-path -> bulk-provide) for resolving bulk bindings on unmarshal
                   submodule-declarations ; resolved-module-path -> module [shared during a module compilation]
                   module-instances    ; (cons resolved-module-path 0-phase) -> namespace [shared among modules]
                   cross-phase-persistent-namespace ; #f or namespace for persistent instances [shared among modules]
                   done-phases)        ; for module instances: phase -> phase (=> done for phase and higher)
        #:property prop:custom-write
        (lambda (ns port mode)
          (write-string "#<namespace" port)
          (define n (namespace-mpi ns))
          (when n
            (fprintf port ":~.s" (format-resolved-module-path-name
                                  (resolved-module-path-name
                                   (module-path-index-resolve n)))))
          (define phase (namespace-phase ns))
          (unless (zero? phase)
            (fprintf port ":~s" phase))
          (write-string ">" port)))

;; Wrapper to make the registry opqaue
(struct module-registry (declarations))

(struct definitions (variables      ; linklet instance
                     transformers   ; sym -> val
                     [instantiated? #:mutable]))

(struct module (self            ; module path index used for a self reference
                requires        ; phase -> list of module-path-index
                provides        ; phase-level -> sym -> binding
                min-phase-level ; phase-level
                max-phase-level ; phase-level
                ;; expected to be consistent with provides and {min,max}-phase-level:
                instantiate     ; namespace phase phase-level ->
                primitive?      ; inline variable values in compiled code?
                cross-phase-persistent?
                root-expand-ctx)) ; preserve module's expand-context for `module->namespace`

(define (make-empty-namespace [share-from-ns #f]
                              #:root-expand-ctx [root-expand-ctx (make-root-expand-context)]
                              #:register? [register? #t])
  (define phase (if share-from-ns
                    (namespace-phase share-from-ns)
                    0))
  (define ns
    (namespace top-level-module-path-index
               root-expand-ctx
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
               (if share-from-ns
                   (namespace-module-instances share-from-ns)
                   (make-hash))
               (and share-from-ns
                    (or (namespace-cross-phase-persistent-namespace share-from-ns)
                        share-from-ns))
               #f))
  (when register?
    (hash-set! (namespace-phase-to-namespace ns) phase ns))
  ns)

(define current-namespace (make-parameter (make-empty-namespace)))

(define (make-module-namespace ns
                               #:mpi name-mpi
                               #:root-expand-context root-expand-ctx
                               #:for-submodule? for-submodule?)
  (define phase 0) ; always start at 0 when compiling a module
  (define name (module-path-index-resolve name-mpi))
  (define m-ns
    ;; Keeps all module declarations, but makes a fresh space of instances
    (struct-copy namespace (make-empty-namespace ns
                                                 #:root-expand-ctx root-expand-ctx
                                                 #:register? #f)
                 [mpi name-mpi]
                 [phase phase]
                 [0-phase phase]
                 [submodule-declarations (if for-submodule?
                                             ;; Same set of submodules:
                                             (namespace-submodule-declarations ns)
                                             ;; Fresh set of submodules:
                                             (make-hash))]
                 [module-instances (make-hash)]
                 [done-phases (make-hasheqv)]))
  (hash-set! (namespace-phase-to-namespace m-ns) phase m-ns)
  (hash-set! (namespace-module-instances m-ns) (cons name phase) m-ns)
  m-ns)

(define (namespace->module ns name)
  (or (hash-ref (namespace-submodule-declarations ns) name #f)
      (hash-ref (module-registry-declarations (namespace-module-registry ns)) name #f)))

(define (make-module self requires provides
                     min-phase-level max-phase-level
                     instantiate
                     #:primitive? [primitive? #f]
                     #:cross-phase-persistent? [cross-phase-persistent? primitive?]
                     #:root-expand-ctx [root-expand-ctx (make-root-expand-context)])
  (module self requires provides
          min-phase-level max-phase-level
          instantiate
          primitive?
          cross-phase-persistent?
          root-expand-ctx))

(define (declare-module! ns m mod-name #:as-submodule? [as-submodule? #f])
  (hash-set! (if as-submodule?
                 (namespace-submodule-declarations ns)
                 (module-registry-declarations (namespace-module-registry ns)))
             mod-name
             m)
  (unless as-submodule?
    ;; Register this module's exports for use in resolving bulk
    ;; bindings, so that bulk bindings can be shared among other
    ;; modules when unmarshaling:
    (register-bulk-provide! (namespace-bulk-binding-registry ns)
                            mod-name
                            (module-self m)
                            (module-provides m)))
  ;; Tell resolver that the module is declared
  ((current-module-name-resolver) mod-name #f)
  ;; If it's a primitive module, add to the table of symbols
  ;; to avoid for bindings
  (when (module-primitive? m)
    (for ([sym (in-hash-keys (hash-ref (module-provides m) 0))])
      (register-built-in-symbol! sym))))

(define (raise-unknown-module-error who mod-name)
  (raise-arguments-error who
                         "unknown module" 
                         "module name" mod-name))

(define (namespace-module-instantiate! ns mpi phase [min-phase 0])
  (unless (module-path-index? mpi)
    (error "not a module path index:" mpi))
  (define name (module-path-index-resolve mpi #t))
  (define m (namespace->module ns name))
  (unless m (raise-unknown-module-error 'instantiate name))
  (cond
   [(and (module-cross-phase-persistent? m)
         (or (not (zero? phase))
             (namespace-cross-phase-persistent-namespace ns)))
    (or (namespace->module-namespace ns name phase)
        (let ([c-ns (or (namespace-cross-phase-persistent-namespace ns)
                        ns)])
          (namespace-module-instantiate! c-ns mpi 0 0)
          (define m-ns (namespace->module-namespace c-ns name 0 #:create? #t))
          (hash-set! (namespace-module-instances ns) (cons name phase) m-ns)
          (for ([(req-phase mods) (in-hash (module-requires m))])
            (for ([mod (in-list mods)])
              (define name (module-path-index-resolve mod))
              (hash-set! (namespace-module-instances ns)
                         (cons name (phase+ phase req-phase))
                         (hash-ref (namespace-module-instances c-ns)
                                   (cons name 0)))))
          m-ns))]
   [else
    (define m-ns (namespace->module-namespace ns name phase #:create? #t))
    (unless ((hash-ref (namespace-done-phases m-ns) phase +inf.0) . <= . min-phase)
      (for ([(req-phase mods) (in-hash (module-requires m))])
        (for ([mod (in-list mods)])
          (namespace-module-instantiate! ns
                                         (module-path-index-shift mod
                                                                  (module-self m)
                                                                  mpi)
                                         (phase+ phase req-phase)
                                         min-phase)))
      (define phase-shift phase) ; base phase = phase shift for instantiation
      (define bulk-binding-registry (namespace-bulk-binding-registry m-ns))
      (for ([phase-level (in-range (module-min-phase-level m)
                                   (add1 (module-max-phase-level m)))])
        (define phase (phase+ phase-level phase-shift))
        (when (phase . >= . min-phase)
          (define defs (namespace->definitions m-ns phase-level))
          (unless (definitions-instantiated? defs)
            (set-definitions-instantiated?! defs #t)
            (define p-ns (namespace->namespace-at-phase m-ns phase))
            ((module-instantiate m) p-ns phase-shift phase-level mpi bulk-binding-registry))))
      (hash-set! (namespace-done-phases m-ns) phase min-phase))]))

(define (namespace-module-visit! ns mpi phase)
  (namespace-module-instantiate! ns mpi phase 1))

(define (namespace->module-namespace ns name 0-phase
                                     #:install!-namespace [install!-ns #f]
                                     #:create? [create? #f]
                                     #:complain-on-failure? [complain-on-failure? #f])
  (or (hash-ref (namespace-module-instances ns) (cons name 0-phase) #f)
      (and complain-on-failure?
           (error "no module instance found:" name 0-phase))
      (and install!-ns
           (let ([m-ns (struct-copy namespace install!-ns
                                    [phase-to-namespace (make-hasheqv)]
                                    [done-phases (make-hasheqv)])])
             (hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
             (hash-set! (namespace-module-instances ns) (cons name 0-phase) m-ns)
             m-ns))
      (and create?
           (let ([m (namespace->module ns name)])
             (unless m
               (error "no module declared to instantiate:" name))
             (define m-ns (struct-copy namespace ns
                                       [mpi (module-self m)]
                                       [root-expand-ctx (module-root-expand-ctx m)]
                                       [phase 0-phase]
                                       [0-phase 0-phase]
                                       [phase-to-namespace (make-hasheqv)]
                                       [phase-level-to-definitions (make-hasheqv)]
                                       [done-phases (make-hasheqv)]))
             (hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
             (hash-set! (namespace-module-instances ns) (cons name 0-phase) m-ns)
             m-ns))))

(define (namespace-same-instance? a-ns b-ns)
  (eq? (namespace-phase-level-to-definitions a-ns)
       (namespace-phase-level-to-definitions b-ns)))

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
        (define d (definitions (make-instance p-ns) (make-hasheq) #f))
        (hash-set! (namespace-phase-level-to-definitions ns) phase-level d)
        d)))

(define (namespace-set-variable! ns phase-level name val)
  (define d (namespace->definitions ns phase-level))
  (set-instance-variable-value! (definitions-variables d) name val))

(define (namespace-set-transformer! ns phase-level name val)
  (define d (namespace->definitions ns phase-level))
  (hash-set! (definitions-transformers d) name val))

(define (namespace-get-variable ns phase-level name fail-k)
  (define d (namespace->definitions ns phase-level))
  (instance-variable-value (definitions-variables d) name fail-k))
  
(define (namespace-get-transformer ns phase-level name fail-k)
  (define d (namespace->definitions ns phase-level))
  (hash-ref (definitions-transformers d) name fail-k))

;; ----------------------------------------

(define (namespace->instance ns phase-shift)
  (definitions-variables (namespace->definitions ns phase-shift)))



(define (namespace-module-use->instance ns mu 
                                        #:shift-from [shift-from #f]
                                        #:shift-to [shift-to #f]
                                        #:phase-shift phase-shift)
  (define mod (module-use-module mu))
  (define m-ns (namespace->module-namespace ns 
                                            (module-path-index-resolve
                                             (if shift-from
                                                 (module-path-index-shift mod shift-from shift-to)
                                                 mod))
                                            phase-shift
                                            #:complain-on-failure? #t))
  (define d (hash-ref (namespace-phase-level-to-definitions m-ns) (module-use-phase mu) #f))
  (if d
      (definitions-variables d)
      (error "namespace mismatch: phase level not found")))
