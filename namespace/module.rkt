#lang racket/base
(require "../common/phase.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/module-path.rkt"
         "../compile/module-use.rkt"
         "../expand/root-expand-context.rkt"
         "../host/linklet.rkt"
         "../compile/built-in-symbol.rkt"
         "namespace.rkt"
         (submod "namespace.rkt" for-module))

(provide make-module-namespace
         raise-unknown-module-error

         namespace->module-instance
         namespace->module-namespace
         namespace-install-module-namespace!
         
         make-module
         declare-module!
         module-self
         module-requires
         module-provides
         module-primitive?
         module-cross-phase-persistent?
         module-no-protected?
         module-inspector
         
         module-instance-namespace
         module-instance-module

         namespace-module-instantiate!
         namespace-module-visit!
         namespace-module-make-available!
         namespace-primitive-module-visit!
         namespace-visit-available-modules!
         namespace-run-available-modules!

         namespace-module-use->instance)

(module+ for-module-reflect
  (provide (struct-out module)))

;; ----------------------------------------

(struct module (self            ; module path index used for a self reference
                requires        ; list of (cons phase list-of-module-path-index)
                provides        ; phase-level -> sym -> binding or protected binding
                language-info   ; #f or vector
                min-phase-level ; phase-level
                max-phase-level ; phase-level
                ;; expected to be consistent with provides and {min,max}-phase-level:
                instantiate     ; namespace phase phase-level ->
                primitive?      ; inline variable values in compiled code?
                cross-phase-persistent?
                no-protected?   ; short cut for checking protected access
                inspector))     ; declaration-time inspector

(define (make-module self requires provides
                     min-phase-level max-phase-level
                     instantiate
                     #:language-info [language-info #f]
                     #:primitive? [primitive? #f]
                     #:cross-phase-persistent? [cross-phase-persistent? primitive?]
                     #:no-protected? [no-protected? #f])
  (module self requires provides language-info
          min-phase-level max-phase-level
          instantiate
          primitive?
          cross-phase-persistent?
          no-protected?
          (current-code-inspector)))

(struct module-instance (namespace
                         module                        ; can be #f for the module being expanded
                         [shifted-requires #:mutable]  ; computed on demand; shifted from `module-requires`
                         phase-level-to-state          ; phase-level -> #f, 'available, or 'started
                         [made-available? #:mutable]   ; no #f in `phase-level-to-state`?
                         data-box))                    ; for use by module implementation

(define (make-module-instance m-ns m)
  (module-instance m-ns           ; namespace
                   m              ; module
                   #f             ; shifted-requires (not yet computed)
                   (make-hasheqv) ; phase-level-to-state
                   #f             ; made-available?
                   (box #f)))     ; data-box

;; ----------------------------------------

;; Create a namespace for expanding a module
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

;; ----------------------------------------

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

;; ----------------------------------------

(define (namespace->module-instance ns name 0-phase
                                    #:install!-namespace [install!-ns #f]
                                    #:complain-on-failure? [complain-on-failure? #f]
                                    #:check-available-at-phase-level [check-available-at-phase-level #f]
                                    #:unavailable-callback [unavailable-callback void])
  (define mi
    (or (hash-ref (namespace-module-instances ns) (cons name 0-phase) #f)
        (let ([c-ns (or (namespace-cross-phase-persistent-namespace ns) ns)])
          (hash-ref (namespace-module-instances c-ns) name #f))
        (and complain-on-failure?
             (error "no module instance found:" name 0-phase))))
  (when (and mi check-available-at-phase-level)
    (check-availablilty mi check-available-at-phase-level unavailable-callback))
  mi)

(define (namespace-install-module-namespace! ns name 0-phase m existing-m-ns)
  (define m-ns (struct-copy namespace ns
                            [mpi (namespace-mpi existing-m-ns)]
                            [root-expand-ctx (namespace-root-expand-ctx existing-m-ns)]
                            [phase (namespace-phase existing-m-ns)]
                            [0-phase (namespace-0-phase existing-m-ns)]
                            [phase-to-namespace (make-hasheqv)]
                            [phase-level-to-definitions (if (module-cross-phase-persistent? m)
                                                            (namespace-phase-level-to-definitions existing-m-ns)
                                                            (make-hasheqv))]
                            [inspector (namespace-inspector existing-m-ns)]))
  (define mi (make-module-instance m-ns m))
  (cond
   [(module-cross-phase-persistent? m)
    (hash-set! (namespace-phase-to-namespace m-ns) 0 m-ns)
    (hash-set! (namespace-module-instances (or (namespace-cross-phase-persistent-namespace ns) ns))
               name
               mi)
    ;; Cross-phase persistent modules normally have only phase 0,
    ;; but '#%core also has phase 1
    (hash-set! (module-instance-phase-level-to-state mi) 0 'started)
    (hash-set! (module-instance-phase-level-to-state mi) 1 'started)]
   [else
    (hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
    (hash-set! (namespace-phase-level-to-definitions m-ns)
               0-phase
               (namespace->definitions existing-m-ns 0-phase))
    (hash-set! (module-instance-phase-level-to-state mi) 0 'started)
    (hash-set! (namespace-module-instances ns) (cons name 0-phase) mi)]))

(define (namespace-create-module-instance! ns name 0-phase m mpi)
  (define m-ns (struct-copy namespace ns
                            [mpi mpi]
                            [root-expand-ctx (box #f)] ; maybe set to non-#f by running phase 0
                            [phase 0-phase]
                            [0-phase 0-phase]
                            [phase-to-namespace (make-hasheqv)]
                            [phase-level-to-definitions (make-hasheqv)]
                            [inspector (make-inspector (module-inspector m))]))
  (hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
  (define mi (make-module-instance m-ns m))
  (hash-set! (namespace-module-instances ns)
             (if (module-cross-phase-persistent? m)
                 name
                 (cons name 0-phase))
             mi)
  mi)

(define (check-availablilty mi check-available-at-phase-level unavailable-callback)
  (define m (module-instance-module mi))
  (when (and m
             (<= (module-min-phase-level m) (add1 check-available-at-phase-level) (module-max-phase-level m))
             (not (hash-ref (module-instance-phase-level-to-state mi) (add1 check-available-at-phase-level) #f)))
    (unavailable-callback)))

(define (namespace->module-namespace ns name 0-phase
                                     #:complain-on-failure? [complain-on-failure? #f]
                                     #:check-available-at-phase-level [check-available-at-phase-level #f]
                                     #:unavailable-callback [unavailable-callback void])
  (define mi (namespace->module-instance ns name 0-phase
                                         #:complain-on-failure? complain-on-failure?
                                         #:check-available-at-phase-level check-available-at-phase-level
                                         #:unavailable-callback unavailable-callback))
  (and mi (module-instance-namespace mi)))

;; ----------------------------------------

;; Create a module instance as needed, and then run the specified phase;
;; see also `run-module-instance!`, below
(define (namespace-module-instantiate! ns mpi instance-phase #:run-phase [run-phase (namespace-phase ns)]
                                       #:skip-run? [skip-run? #f]
                                       #:otherwise-available? [otherwise-available? #t]
                                       #:seen [seen #hasheq()])
  (unless (module-path-index? mpi)
    (error "not a module path index:" mpi))
  (define name (module-path-index-resolve mpi #t))
  (define m (namespace->module ns name))
  (unless m (raise-unknown-module-error 'instantiate name))
  (define (instantiate! instance-phase run-phase ns)
    ;; Get or create a namespace for the module+phase combination:
    (define mi (or (namespace->module-instance ns name instance-phase)
                   (namespace-create-module-instance! ns name instance-phase m mpi)))
    (run-module-instance! mi ns #:run-phase run-phase
                          #:skip-run? skip-run?
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

(define (namespace-module-make-available! ns mpi instance-phase #:visit-phase [visit-phase (namespace-phase ns)])
  (namespace-module-instantiate! ns mpi instance-phase #:run-phase (add1 visit-phase) #:skip-run? #t))

;; The `instance-phase` corresponds to the phase shift for the module
;; instances. The module may have content at different phase levels,
;; which are all consistently shifted. The `run-phase` is an absolute
;; phase that should be immediately run, unless `skip-run?` is treu;
;; to put it another way, phase level `(phase- instance-phase
;; run-phase)` within the instance should be run immediately.
;; Normally, the instance is made available at all other non-negative
;; phases, but `#:otherwise-available?` controls that behavior.
(define (run-module-instance! mi ns #:run-phase run-phase
                              #:skip-run? skip-run? 
                              #:otherwise-available? otherwise-available?
                              #:seen [seen #hasheq()])
  ;; Nothing to do if we've run this phase already and made the
  ;; instance sufficiently available:
  (define m-ns (module-instance-namespace mi))
  (define instance-phase (namespace-0-phase m-ns))
  (define run-phase-level (phase- run-phase instance-phase))
  (unless (and (or skip-run?
                   (eq? 'started (hash-ref (module-instance-phase-level-to-state mi) run-phase-level #f)))
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
                                       #:skip-run? skip-run?
                                       #:otherwise-available? otherwise-available?
                                       #:seen (hash-set seen mpi #t))))
    
    ;; Run or make available phases of the module body:
    (unless (label-phase? instance-phase)
      (for ([phase-level (in-range (module-max-phase-level m) (sub1 (module-min-phase-level m)) -1)])
        (define phase (phase+ phase-level phase-shift))
        (cond
         [(and (not skip-run?)
               (eqv? phase run-phase))
          ;; This is the phase to make sure that we've run
          (unless (eq? 'started (hash-ref (module-instance-phase-level-to-state mi) phase-level #f))
            (hash-set! (module-instance-phase-level-to-state mi) phase-level 'started)
            (define defs (namespace->definitions m-ns phase-level))
            (define p-ns (namespace->namespace-at-phase m-ns phase))
            (define insp (module-inspector m))
            (define go (module-instantiate m))
            (go (module-instance-data-box mi) p-ns phase-shift phase-level mpi bulk-binding-registry insp))]
         [(and otherwise-available?
               (not (negative? run-phase))
               (not (hash-ref (module-instance-phase-level-to-state mi) phase-level #f)))
          ;; This is a phase to merely make available
          (hash-update! (namespace-available-module-instances ns)
                        phase
                        (lambda (l) (cons mi l))
                        null)
          (hash-set! (module-instance-phase-level-to-state mi) phase-level 'available)])))

    (when otherwise-available?
      (set-module-instance-made-available?! mi #t))

    (unless skip-run?
      ;; In case there's no such phase for this module instance, claim 'started
      ;; to short-circuit future attempts:
      (hash-set! (module-instance-phase-level-to-state mi) run-phase-level 'started))))

(define (namespace-visit-available-modules! ns [run-phase (namespace-phase ns)])
  (namespace-run-available-modules! ns (add1 run-phase)))

(define (namespace-run-available-modules! ns [run-phase (namespace-phase ns)])
  (let loop ()
    (define mis (hash-ref (namespace-available-module-instances ns) run-phase null))
    (unless (null? mis)
      (hash-set! (namespace-available-module-instances ns) run-phase null)
      (for ([mi (in-list (reverse mis))])
        (run-module-instance! mi ns #:run-phase run-phase #:skip-run? #f #:otherwise-available? #f))
      ;; In case instantiation added more reflectively:
      (loop))))

(define (namespace-primitive-module-visit! ns name)
  (define mi (hash-ref (namespace-module-instances ns) (make-resolved-module-path name)))
  (run-module-instance! mi ns #:run-phase 1 #:skip-run? #f #:otherwise-available? #t))

;; ----------------------------------------

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

