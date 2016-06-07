#lang racket/base
(require "namespace.rkt"
         "phase.rkt"
         "module-use.rkt"
         "module-path.rkt"
         "serialize.rkt"
         "linklet.rkt"
         "compile-instance.rkt"
         (only-in "eval-compiled-top.rkt" eval-linklets)
         "compiled-in-memory.rkt")

;; Run a reprsentation of top-level code as produced by `compile-module`;
;; see "compile.rkt" and "compile-module.rkt"

(provide eval-module)

(define (eval-module c
                     #:namespace [ns (current-namespace)]
                     #:as-submodule? [as-submodule? #f])
  (define ld (if (compiled-in-memory? c)
                 (compiled-in-memory-linklet-directory c)
                 c))
  (define h (eval-linklets (linklet-directory->hash ld)))

  (define data-instance
    (if (compiled-in-memory? c)
        (make-data-instance-from-compiled-in-memory c)
        (instantiate-linklet (hash-ref h #".data")
                             (list deserialize-instance))))

  (define declaration-instance
    (instantiate-linklet (hash-ref h #".decl")
                         (list deserialize-instance
                               data-instance)))
  
  (define (decl key)
    (instance-variable-value declaration-instance key))
  
  (define (declare-submodules names pre?)
    (if (compiled-in-memory? c)
        (for ([c (in-list (if pre?
                              (compiled-in-memory-pre-compiled-in-memorys c)
                              (compiled-in-memory-post-compiled-in-memorys c)))])
          (eval-module c #:namespace ns))
        (for ([name (in-list names)])
          (define sm-cd (hash-ref h (encode-linklet-directory-key name)))
          (unless sm-cd (error "missing submodule declaration:" name))
          (eval-module sm-cd #:namespace ns))))
  
  (unless as-submodule?
    (declare-submodules (decl 'pre-submodules) #t))
  
  (define root-module-name (instance-variable-value declaration-instance 'root-module-name))
  
  (define original-self (decl 'self-mpi))
   
  (define m (make-module original-self
                         (decl 'requires)
                         (decl 'provides)
                         (decl 'min-phase)
                         (decl 'max-phase)
                         (lambda (ns phase-shift phase-level self bulk-binding-registry)
                           (define cu (hash-ref h (encode-linklet-directory-key phase-level) #f))
                           (when cu
                             (define imports
                               (for/list ([mu (in-list (hash-ref (decl 'phase-to-link-modules) phase-level))])
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
                                #:set-transformer! (lambda (name val)
                                                     (namespace-set-transformer! ns (sub1 phase-level) name val))))
                             (instantiate-linklet cu (list* deserialize-instance
                                                            data-instance
                                                            inst
                                                            imports)
                                                  (namespace->instance ns phase-level))))
                         #:cross-phase-persistent? (decl 'cross-phase-persistent?)))

  (declare-module! ns
                   m
                   (substitute-module-declare-name (decl 'root-module-name)
                                                   (decl 'default-name))
                   #:as-submodule? as-submodule?)

  (unless as-submodule?
    (declare-submodules (decl 'post-submodules) #f)))

;; ----------------------------------------

(define (make-data-instance-from-compiled-in-memory cim)
  (define data-instance (make-instance 'data))
  (instance-set-variable-value! data-instance 'mpi-vector
                                (compiled-in-memory-mpis cim))
  (instance-set-variable-value! data-instance 'deserialized-syntax
                                (compiled-in-memory-syntax-literalss cim))
  data-instance)
