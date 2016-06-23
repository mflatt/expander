#lang racket/base
(require "../common/phase.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../compile/module-use.rkt"
         "../host/linklet.rkt"
         "../compile/serialize.rkt"
         "../compile/instance.rkt"
         "../compile/eager-instance.rkt"
         "../compile/compiled-in-memory.rkt"
         "../compile/multi-top.rkt"
         "top-level-instance.rkt"
         "protect.rkt")

;; Run a representation of top-level code as produced by `compile-top`;
;; see "compile/main.rkt", "compile/top.rkt", and "compile/multi-top.rkt"

(provide eval-top)

(define (eval-top c ns [eval-compiled eval-top])
  (define ld (if (compiled-in-memory? c)
                 (compiled-in-memory-linklet-directory c)
                 c))
  (if (hash-ref (linklet-directory->hash ld) #f #f)
      (eval-single-top c ns)
      (eval-multiple-tops c ns eval-compiled)))

(define (eval-multiple-tops c ns eval-compiled)
  (cond
   [(compiled-in-memory? c)
    (let loop ([cims (compiled-in-memory-pre-compiled-in-memorys c)])
      (cond
       [(null? cims) void]
       [(null? (cdr cims))
        ;; Tail call:
        (eval-compiled (car cims) ns)]
       [else
        (eval-compiled (car cims) ns)
        (loop (cdr cims))]))]
   [else
    (let loop ([lds (compiled-top->compiled-tops c)])
      (cond
       [(null? lds) (void)]
       [(null? (cdr lds))
        ;; Tail call:
        (eval-compiled (car lds) ns)]
       [else
        (eval-compiled (car lds) ns)
        (loop (cdr lds))]))]))

(define (eval-single-top c ns)
  (define ld (if (compiled-in-memory? c)
                 (compiled-in-memory-linklet-directory c)
                 c))
  (define h (linklet-bundle->hash (hash-ref (linklet-directory->hash ld) #f)))
  (define link-instance
    (if (compiled-in-memory? c)
        (link-instance-from-compiled-in-memory c)
        (instantiate-linklet (eval-linklet (hash-ref h 'link))
                             (list deserialize-instance
                                   (make-eager-instance-instance
                                    #:namespace ns
                                    #:dest-phase (namespace-phase ns)
                                    #:self (namespace-mpi ns)
                                    #:bulk-binding-registry (namespace-bulk-binding-registry ns)
                                    #:inspector (current-code-inspector))))))

  (define orig-phase (instance-variable-value link-instance 'original-phase))
  (define max-phase (instance-variable-value link-instance 'max-phase))
  (define phase-shift (phase- (namespace-phase ns) orig-phase))

  (define extra-inspector (and (compiled-in-memory? c)
                               (compiled-in-memory-compile-time-inspector c)))
  (define phase-to-link-extra-inspectorsss
    (if (compiled-in-memory? c)
        (compiled-in-memory-phase-to-link-extra-inspectorsss c)
        #hasheqv()))

  ;; Call the last thunk in tail position:
  ((for/fold ([prev-thunk void]) ([phase (in-range max-phase (sub1 orig-phase) -1)])
     (prev-thunk) ;; call a not-last thunk before proceeding with the next phase
     
     (define module-uses (hash-ref (instance-variable-value link-instance 'phase-to-link-modules)
                                   phase
                                   null))
     (define-values (import-module-instances import-instances)
       (for/lists (mis is) ([mu (in-list module-uses)])
         (namespace-module-use->module+linklet-instances
          ns mu #:phase-shift (phase- (phase+ phase phase-shift)
                                      (module-use-phase mu)))))

     (define phase-ns (namespace->namespace-at-phase ns (phase+ phase phase-shift)))
     
     (define inst (make-instance-instance
                   #:namespace phase-ns
                   #:phase-shift phase-shift
                   #:self (namespace-mpi ns)
                   #:bulk-binding-registry (namespace-bulk-binding-registry ns)
                   #:inspector (namespace-inspector ns)
                   #:set-transformer! (lambda (name val)
                                        (namespace-set-transformer! ns
                                                                    (phase+ (sub1 phase) phase-shift)
                                                                    name
                                                                    val))))

     (define linklet
       (let ([l (hash-ref h phase #f)])
         (and l (eval-linklet l))))

     (cond
      [linklet
       (check-require-access linklet #:skip-imports 3
                             module-uses import-module-instances (current-code-inspector)
                             extra-inspector
                             (hash-ref phase-to-link-extra-inspectorsss phase #f))
       (define (instantiate)
         (parameterize ([current-namespace (if (zero-phase? phase)
                                               (current-namespace)
                                               phase-ns)])
           ;; Providing a target instance to `instantiate-linklet` means that we get
           ;; the body's results instead of the instance as a result
           (instantiate-linklet linklet
                                (list* top-level-instance
                                       link-instance
                                       inst
                                       import-instances)
                                ;; Instantiation merges with the namespace's current instance:
                                (namespace->instance ns (phase+ phase phase-shift)))))
       ;; Return `instantiate` as the next thunk
       (cond
        [(eqv? phase orig-phase)
         (if (zero-phase? phase)
             instantiate
             (lambda () (parameterize ([current-namespace phase-ns])
                     (instantiate))))]
        [else instantiate])]
      [else void]))))

(define (link-instance-from-compiled-in-memory cim)
  (define link-instance (make-instance 'link))
  (instance-set-variable-value! link-instance 'original-phase
                                (compiled-in-memory-phase cim))
  (instance-set-variable-value! link-instance 'max-phase
                                (compiled-in-memory-max-phase cim))
  (instance-set-variable-value! link-instance 'phase-to-link-modules
                                (compiled-in-memory-phase-to-link-module-uses cim))
  (instance-set-variable-value! link-instance 'mpi-vector
                                (compiled-in-memory-mpis cim))
  (instance-set-variable-value! link-instance 'syntax-literalss
                                (compiled-in-memory-syntax-literalss cim))
  link-instance)
