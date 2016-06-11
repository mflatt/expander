#lang racket/base
(require "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/module-path.rkt"
         "../compile/module-use.rkt"
         "../expand/root-expand-context.rkt"
         "../host/linklet.rkt"
         "../compile/built-in-symbol.rkt")

(provide make-namespace
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
         namespace-primitive-module-visit!
         namespace-visit-available-modules!
         namespace-run-available-modules!
         namespace->module-namespace
         namespace-same-instance?
         
         namespace-set-variable!
         namespace-unset-variable!
         namespace-set-transformer!
         namespace-get-variable
         namespace-get-transformer
         
         namespace->instance
         namespace-module-use->instance)

(struct namespace (mpi                 ; module path index (that's already resolved); instance-specific for a module
                   root-expand-ctx     ; module context for top-level expansion
                   phase               ; phase (not phase level!) of this namespace
                   0-phase             ; phase of module instance's phase-level 0
                   phase-to-namespace  ; phase -> namespace for same module  [shared for the same module instance]
                   phase-level-to-definitions ; phase-level -> definitions [shared for the same module instance]
                   module-registry     ; module-registry of (resolved-module-path -> module) [shared among modules]
                   bulk-binding-registry ; (resolved-module-path -> bulk-provide) for resolving bulk bindings on unmarshal
                   submodule-declarations ; resolved-module-path -> module [shared during a module compilation]
                   cross-phase-persistent-namespace ; #f or namespace for persistent instances [shared among modules]
                   available-module-instances  ; phase -> list of module-instance [shared among modules]
                   module-instances)  ; instance-key -> module-instance [shared among modules]
        ;;                            ;   where an instance-key is either (cons resolved-module-path 0-phase)
        ;;                            ;   or just a resolved-module-path, the latter for cross phase persistent modules
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
                     transformers   ; sym -> val
                     [instantiated? #:mutable]))

(struct module-instance (namespace
                         module
                         [shifted-requires #:mutable]  ; computed on demand; shifted from `module-requires`
                         phase-level-to-state          ; phase-level -> #f, 'available, or 'started
                         [made-available? #:mutable])) ; no #f in `phase-level-to-state`?

(struct module (self            ; module path index used for a self reference
                requires        ; list of (cons phase list-of-module-path-index)
                provides        ; phase-level -> sym -> binding
                min-phase-level ; phase-level
                max-phase-level ; phase-level
                ;; expected to be consistent with provides and {min,max}-phase-level:
                instantiate     ; namespace phase phase-level ->
                primitive?      ; inline variable values in compiled code?
                cross-phase-persistent?
                root-expand-ctx)) ; preserve module's expand-context for `module->namespace`

(define (make-namespace [share-from-ns #f]
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
               (and share-from-ns
                    (or (namespace-cross-phase-persistent-namespace share-from-ns)
                        share-from-ns))
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

(define (make-module-namespace ns
                               #:mpi name-mpi
                               #:root-expand-context root-expand-ctx
                               #:for-submodule? for-submodule?)
  (define phase 0) ; always start at 0 when compiling a module
  (define name (module-path-index-resolve name-mpi))
  (define m-ns
    ;; Keeps all module declarations, but makes a fresh space of instances
    (struct-copy namespace (make-namespace ns
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
                 [available-module-instances (make-hash)]
                 [module-instances (make-hash)]))
  (hash-set! (namespace-phase-to-namespace m-ns) phase m-ns)
  (hash-set! (namespace-module-instances m-ns) (cons name phase) (make-module-instance m-ns #f))
  m-ns)

(define (make-module-instance m-ns m)
  (module-instance m-ns m #f (make-hasheqv) #f))

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

;; Create a module instance as needed, and then run the specified phase;
;; see also `run-module-instance!`, below
(define (namespace-module-instantiate! ns mpi instance-phase #:run-phase [run-phase (namespace-phase ns)]
                                       #:otherwise-available? [otherwise-available? #t]
                                       #:seen [seen #hasheq()])
  (unless (module-path-index? mpi)
    (error "not a module path index:" mpi))
  (define name (module-path-index-resolve mpi #t))
  (define m (namespace->module ns name))
  (unless m (raise-unknown-module-error 'instantiate name))
  (define (instantiate! instance-phase run-phase ns)
    ;; Get or create a namespace for the module+phase combination:
    (define mi (namespace->module-instance
                ns name instance-phase
                #:create-mpi mpi
                #:add-as-cross-phase-persistent? (module-cross-phase-persistent? m)))
    (run-module-instance! mi ns #:run-phase run-phase
                          #:otherwise-available? otherwise-available?
                          #:seen seen))
  ;; If the module is cross-phase persistent, make sure it's instantiated
  ;; at phase 0 and registered in `ns` as phaseless; otherwise
  (cond
   [(module-cross-phase-persistent? m)
    (instantiate! 0 0 (or (namespace-cross-phase-persistent-namespace ns) ns))]
   [else
    (instantiate! instance-phase run-phase ns)]))

(define (namespace-module-visit! ns mpi instance-phase #:visit-phase [visit-phase (namespace-phase ns)])
  (namespace-module-instantiate! ns mpi instance-phase #:run-phase (add1 visit-phase)))

;; The `instance-phase` corresponds to the phase shift for the module
;; instances. The module may have content at different phase levels,
;; which are all consistently shifted. The `run-phase` is an absolute
;; phase that should be immediately run; to put it another way, phase
;; level `(phase- instance-phase run-phase)` within the instance
;; should be run immediately. The instance should at most be made
;; available at all other non-negative phases, but
;; `#:otherwise-available?` controls that behavior.
(define (run-module-instance! mi ns #:run-phase run-phase
                              #:otherwise-available? otherwise-available?
                              #:seen [seen #hasheq()])
  ;; Nothing to do if we've run this phase already and made the
  ;; instance sufficiently available:
  (define m-ns (module-instance-namespace mi))
  (define instance-phase (namespace-0-phase m-ns))
  (define run-phase-level (phase- run-phase instance-phase))
  (unless (and (eq? 'started (hash-ref (module-instance-phase-level-to-state mi) run-phase-level #f))
               (or (not otherwise-available?)
                   (module-instance-made-available? mi)))
    ;; Something to do...
    (define m (module-instance-module mi))
    (define mpi (namespace-mpi m-ns))
    (define phase-shift instance-phase) ; instance phase = phase shift
    (define bulk-binding-registry (namespace-bulk-binding-registry m-ns))
    
    (when (hash-ref seen mi #f)
      (error 'require "import cycle detected during module instantiation"))
    
    ;; If we haven't shifted required mpi's already, do that:
    (unless (module-instance-shifted-requires mi)
      (set-module-instance-shifted-requires!
       mi
       (for/list ([phase+mpis (in-list (module-requires m))])
         (cons (car phase+mpis)
               (for/list ([req-mpi (in-list (cdr phase+mpis))])
                 (module-path-index-shift req-mpi
                                          (module-self m)
                                          mpi))))))
    
    ;; Recur for required modules:
    (for ([phase+mpis (in-list (module-instance-shifted-requires mi))])
      (define req-phase (car phase+mpis))
      (for ([req-mpi (in-list (cdr phase+mpis))])
        (namespace-module-instantiate! ns req-mpi (phase+ instance-phase req-phase)
                                       #:run-phase run-phase
                                       #:otherwise-available? otherwise-available?
                                       #:seen (hash-set seen mpi #t))))
    
    ;; Run or make available phases of the module body:
    (unless (label-phase? instance-phase)
      (for ([phase-level (in-range (module-max-phase-level m) (sub1 (module-min-phase-level m)) -1)])
        (define phase (phase+ phase-level phase-shift))
        (cond
         [(eqv? phase run-phase)
          ;; This is the phase to make sure that we've run
          (unless (eq? 'started (hash-ref (module-instance-phase-level-to-state mi) phase-level #f))
            (hash-set! (module-instance-phase-level-to-state mi) phase-level 'started)
            (define defs (namespace->definitions m-ns phase-level))
            (unless (definitions-instantiated? defs)
              (set-definitions-instantiated?! defs #t)
              (define p-ns (namespace->namespace-at-phase m-ns phase))
              ((module-instantiate m) p-ns phase-shift phase-level mpi bulk-binding-registry)))]
         [(and otherwise-available?
               (not (negative? phase))
               (not (hash-ref (module-instance-phase-level-to-state mi) phase-level #f)))
          ;; This is a phase to merely make available
          (hash-update! (namespace-available-module-instances ns)
                        phase
                        (lambda (l) (cons mi l))
                        null)
          (hash-set! (module-instance-phase-level-to-state mi) phase-level 'available)])))

    (when otherwise-available?
      (set-module-instance-made-available?! mi #t))

    ;; In case there's no such phase for this module instance, claim 'started
    ;; to short-circuit future attempts:
    (hash-set! (module-instance-phase-level-to-state mi) run-phase-level 'started)))

(define (namespace-visit-available-modules! ns [run-phase (namespace-phase ns)])
  (namespace-run-available-modules! ns (add1 run-phase)))

(define (namespace-run-available-modules! ns [run-phase (namespace-phase ns)])
  (let loop ()
    (define mis (hash-ref (namespace-available-module-instances ns) run-phase null))
    (unless (null? mis)
      (hash-set! (namespace-available-module-instances ns) run-phase null)
      (for ([mi (in-list (reverse mis))])
        (run-module-instance! mi ns #:run-phase run-phase #:otherwise-available? #f))
      ;; In case instantiation added more reflectively:
      (loop))))

(define (namespace-primitive-module-visit! ns name)
  (define mi (hash-ref (namespace-module-instances ns) (make-resolved-module-path name)))
  (run-module-instance! mi ns #:run-phase 1
                        #:otherwise-available? #t))

(define (namespace->module-namespace ns name 0-phase
                                     #:install!-namespace [install!-ns #f]
                                     #:add-as-cross-phase-persistent? [add-as-cross-phase-persistent? #t]
                                     #:complain-on-failure? [complain-on-failure? #f]
                                     #:check-available-at-phase-level [check-available-at-phase-level #f]
                                     #:unavailable-callback [unavailable-callback void])
  (define mi (namespace->module-instance ns name 0-phase
                                         #:install!-namespace install!-ns
                                         #:add-as-cross-phase-persistent? add-as-cross-phase-persistent?
                                         #:complain-on-failure? complain-on-failure?))
  (when (and mi check-available-at-phase-level)
    (check-availablilty mi check-available-at-phase-level unavailable-callback))
  (and mi (module-instance-namespace mi)))

(define (namespace->module-instance ns name 0-phase
                                    #:install!-namespace [install!-ns #f]
                                    #:create-mpi [create-mpi #f]
                                    #:add-as-cross-phase-persistent? [add-as-cross-phase-persistent? #t]
                                    #:complain-on-failure? [complain-on-failure? #f])
  (or (hash-ref (namespace-module-instances ns) (cons name 0-phase) #f)
      (let ([c-ns (or (namespace-cross-phase-persistent-namespace ns) ns)])
        (hash-ref (namespace-module-instances c-ns) name #f))
      (and complain-on-failure?
           (error "no module instance found:" name 0-phase))
      (and install!-ns
           (let ([m-ns (struct-copy namespace install!-ns
                                    [phase-to-namespace (make-hasheqv)])])
             (hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
             (define mi (make-module-instance m-ns (namespace->module ns name)))
             (hash-set! (module-instance-phase-level-to-state mi) 0 'started)
             (hash-set! (namespace-module-instances ns)
                        (if add-as-cross-phase-persistent?
                            name
                            (cons name 0-phase))
                        mi)
             mi))
      (and create-mpi
           (let ([m (namespace->module ns name)])
             (unless m
               (error "no module declared to instantiate:" name))
             (define m-ns (struct-copy namespace ns
                                       [mpi create-mpi]
                                       [root-expand-ctx (module-root-expand-ctx m)]
                                       [phase 0-phase]
                                       [0-phase 0-phase]
                                       [phase-to-namespace (make-hasheqv)]
                                       [phase-level-to-definitions (make-hasheqv)]))
             (hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
             (define mi (make-module-instance m-ns m))
             (hash-set! (namespace-module-instances ns)
                        (if add-as-cross-phase-persistent?
                            name
                            (cons name 0-phase))
                        mi)
             mi))))

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
  (instance-set-variable-value! (definitions-variables d) name val))

(define (namespace-unset-variable! ns phase-level name)
  (define d (namespace->definitions ns phase-level))
  (instance-unset-variable! (definitions-variables d) name))

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

(define (check-availablilty mi check-available-at-phase-level unavailable-callback)
  (define m (module-instance-module mi))
  (when (and m
             (<= (module-min-phase-level m) (add1 check-available-at-phase-level) (module-max-phase-level m))
             (not (hash-ref (module-instance-phase-level-to-state mi) (add1 check-available-at-phase-level) #f)))
    (unavailable-callback)))

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
