#lang racket/base

(provide make-empty-namespace
         current-namespace
         namespace->module
         
         make-module
         declare-module!
         
         namespace-module-instantiate!
         namespace-module-visit!
         namespace->module-namespace
         
         namespace-set-variable!
         namespace-set-transformer!
         namespace-get-variable
         namespace-get-transformer)

;; A "phase" is the phase at which a module is instantiated.
;; A "phase level" is a phase relative to a module's body.

(struct namespace (phases              ; phase-level -> definitions
                   module-declarations ; resolved-module-name -> module
                   module-instances))  ; (cons resolved-module-name phase) -> namespace

(struct definitions (variables      ; sym -> val
                     transformers   ; sym -> val
                     [instantiated? #:mutable]))

(struct module (imports        ; list of (cons resolved-module-name phase-level)
                variable-exports    ; phase-level -> sym -> binding
                transformer-exports ; phase-level -> sym -> binding
                ;; expected to be consistent with exports:
                instantiate))  ; namespace phase phase-level ->

(define current-namespace (make-parameter (namespace
                                           (make-hasheqv)
                                           (make-hasheq)
                                           (make-hash))))

(define (namespace->module ns name)
  (hash-ref (namespace-module-declarations ns) name #f))

(define (make-module imports variable-exports transformer-exports instantiate)
  (module imports variable-exports transformer-exports instantiate))

(define (declare-module! ns name m)
  (hash-set! (namespace-module-declarations ns) name m))

(define (namespace-module-instantiate! ns name phase phase-level)
  (define m-ns (namespace->module-namespace ns name phase #:create? #t))
  (define m (namespace->module ns name))
  (define defs (namespace->definitions m-ns phase-level))
  (unless (definitions-instantiated? defs)
    (set-definitions-instantiated?! defs #t)
    ((module-instantiate m) m-ns phase phase-level)))

(define (namespace-module-visit! ns name phase phase-level)
  (namespace-module-instantiate! ns name phase (add1 phase-level)))

(define (namespace->module-namespace ns name phase #:create? [create? #f])
  (or (hash-ref (namespace-module-instances ns) (cons name phase) #f)
      (and create?
           (let ([m (namespace->module ns name)])
             (unless m
               (error "no module declared to instantiate:" name))
             (define m-ns (namespace (make-hasheqv)
                                     (namespace-module-declarations ns)
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
