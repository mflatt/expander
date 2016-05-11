#lang racket/base
(require "phase.rkt"
         "scope.rkt"
         "module-path.rkt")

(provide make-empty-namespace
         current-namespace
         make-module-namespace
         namespace->module
         
         namespace-syntax-introduce
         namespace-scope
         
         make-module
         remake-module
         declare-module!
         module-self
         module-requires
         module-provides

         namespace-module-instantiate!
         namespace-module-visit!
         namespace->module-namespace
         
         namespace-set-variable!
         namespace-set-transformer!
         namespace-get-variable
         namespace-get-transformer)

(struct namespace (scope               ; scope for top-level bindings
                   phases              ; phase-level -> definitions
                   module-declarations ; resolved-module-path -> module
                   submodule-declarations ; resolved-module-path -> module
                   module-instances))  ; (cons resolved-module-path phase) -> namespace

(struct definitions (variables      ; sym -> val
                     transformers   ; sym -> val
                     [instantiated? #:mutable]))

(struct module (self            ; module path index used for a self reference
                requires        ; phase -> list of module-path-index
                provides        ; phase-level -> sym -> binding
                min-phase-level ; phase-level
                max-phase-level ; phase-level
                ;; expected to be consistent with provides and {min,max}-phase-level:
                instantiate))  ; namespace phase phase-level ->

(define (make-empty-namespace)
  (namespace (new-multi-scope)
             (make-hasheqv)
             (make-hasheq)
             (make-hasheq)
             (make-hash)))

(define current-namespace (make-parameter (make-empty-namespace)))

(define (namespace-syntax-introduce s [ns (current-namespace)])
  (add-scope s (namespace-scope ns)))

(define (make-module-namespace ns name for-submodule?)
  (define m-ns
    ;; Keeps all module declarations, but makes a fresh space of instances
    (struct-copy namespace (make-empty-namespace)
                 [module-declarations (namespace-module-declarations ns)]
                 [submodule-declarations (if for-submodule?
                                             ;; Same set of submodules:
                                             (namespace-submodule-declarations ns)
                                             ;; Fresh set of submodules:
                                             (make-hash))]))
  (hash-set! (namespace-module-instances m-ns) (cons name 0) m-ns)
  m-ns)

(define (namespace->module ns name)
  (or (hash-ref (namespace-submodule-declarations ns) name #f)
      (hash-ref (namespace-module-declarations ns) name #f)))

(define (make-module self requires provides
                     min-phase-level max-phase-level
                     instantiate)
  (module self requires provides
          min-phase-level max-phase-level
          instantiate))

(define (remake-module m self requires provides)
  (struct-copy module m
               [self self]
               [requires requires]
               [provides provides]))

(define (declare-module! ns m #:as-submodule? [as-submodule? #f])
  (hash-set! (if as-submodule?
                 (namespace-submodule-declarations ns)
                 (namespace-module-declarations ns))
             (module-path-index-resolve (module-self m))
             m))

(define (namespace-module-instantiate! ns name phase-shift [min-phase 0])
  (unless (resolved-module-path? name)
    (error "not a resolved module path:" name))
  (define m-ns (namespace->module-namespace ns name phase-shift #:create? #t))
  (define m (namespace->module ns name))
  (for ([(req-phase mods) (in-hash (module-requires m))])
    (for ([mod (in-list mods)])
      (namespace-module-instantiate! ns (module-path-index-resolve mod)
                                     (phase+ phase-shift req-phase)
                                     min-phase)))
  (for ([phase-level (in-range (module-min-phase-level m)
                               (add1 (module-max-phase-level m)))])
    (define phase (phase+ phase-level phase-shift))
    (when (phase . >= . min-phase)
      (define defs (namespace->definitions m-ns phase-level))
      (unless (definitions-instantiated? defs)
        (set-definitions-instantiated?! defs #t)
        ((module-instantiate m) m-ns phase-shift phase-level (module-self m))))))

(define (namespace-module-visit! ns name phase)
  (namespace-module-instantiate! ns name phase 1))

(define (namespace->module-namespace ns name phase
                                     #:create? [create? #f]
                                     #:complain-on-failure? [complain-on-failure? #f])
  (or (hash-ref (namespace-module-instances ns) (cons name phase) #f)
      (and complain-on-failure?
           (error "no module instance found:" name))
      (and create?
           (let ([m (namespace->module ns name)])
             (unless m
               (error "no module declared to instantiate:" name))
             (define m-ns (namespace (new-multi-scope)
                                     (make-hasheqv)
                                     (namespace-module-declarations ns)
                                     (namespace-submodule-declarations ns)
                                     (namespace-module-instances ns)))
             (hash-set! (namespace-module-instances ns) (cons name phase) m-ns)
             m-ns))))

(define (namespace->definitions ns phase-level)
  (define d (hash-ref (namespace-phases ns) phase-level #f))
  (or d
      (let ([d (definitions (make-hasheq) (make-hasheq) #f)])
        (hash-set! (namespace-phases ns) phase-level d)
        d)))

(define (namespace-set-variable! ns phase-level name val)
  (define d (namespace->definitions ns phase-level))
  (hash-set! (definitions-variables d) name val))

(define (namespace-set-transformer! ns phase-level name val)
  (define d (namespace->definitions ns phase-level))
  (hash-set! (definitions-transformers d) name val))

(define (namespace-get-variable ns phase-level name fail-k)
  (define d (namespace->definitions ns phase-level))
  (hash-ref (definitions-variables d) name fail-k))

(define (namespace-get-transformer ns phase-level name fail-k)
  (define d (namespace->definitions ns phase-level))
  (hash-ref (definitions-transformers d) name fail-k))
