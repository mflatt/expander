#lang racket/base
(require "compiled-top.rkt"
         "phase.rkt"
         "namespace.rkt"
         "module-use.rkt"
         "linklet.rkt"
         "serialize.rkt"
         "compile-instance.rkt")

;; Run a reprsentation of top-level code as produced by `compile-top`;
;; see "compile.rkt"

(provide eval-top-from-linklet-directory
         eval-top-from-compiled-top
         
         eval-linklets)

(define (eval-top-from-linklet-directory cd ns)
  (eval-top-from-compiled-top (compiled-top cd #f #f #f #f)
                              ns))

(define (eval-top-from-compiled-top ct ns)
  (define cd (compiled-top-linklet-directory ct))
  (define h (eval-linklets (linklet-directory->hash cd)))
  (define link-instance
    (and (not (compiled-top-phase-to-link-module-uses ct))
         (instantiate-linklet (hash-ref h #"link")
                              (list deserialize-instance))))
  (define phase (namespace-phase ns))
  (define imports
    (for/list ([mu (or (hash-ref (or (compiled-top-phase-to-link-module-uses ct) #hasheqv())
                                 (compiled-top-phase ct))
                       (instance-variable-value link-instance 'link-modules))])
      (namespace-module-use->instance ns mu #:phase-shift (phase- phase (module-use-phase mu)))))
  
  (define phase-shift (phase- (namespace-phase ns)
                              (or (compiled-top-phase ct)
                                  (instance-variable-value link-instance 'original-phase))))
  
  (define inst (make-instance-instance
                #:namespace ns
                #:phase-shift phase-shift
                #:self (namespace-mpi ns)
                #:bulk-binding-registry (namespace-bulk-binding-registry ns)
                #:set-transformer! (lambda (name val)
                                     (namespace-set-transformer! ns phase-shift name val))))

  (define i
    (instantiate-linklet (hash-ref h (encode-linklet-directory-key phase))
                         (list* (or link-instance
                                    (compiled-top-make-link-instance ct phase-shift))
                                inst
                                imports)
                         ;; Instantiation merges with the namespace's current instance:
                         (namespace->instance ns (namespace-phase ns))))
  
  ((instance-variable-value i 'body-thunk)))
  

(define (compiled-top-make-link-instance ct phase-shift)
  (define link-instance (make-instance 'top))
  (set-instance-variable-value! link-instance 'mpi-vector
                                ((compiled-top-get-mpis ct)))
  (set-instance-variable-value! link-instance 'syntax-literals
                                ((hash-ref (compiled-top-phase-to-get-syntax-literals ct)
                                           (compiled-top-phase ct))
                                 phase-shift))
  (set-instance-variable-value! link-instance 'get-syntax-literal!
                                (lambda (pos)
                                  (error "internal error: missing syntax literal?" pos)))
  link-instance)

;; ----------------------------------------

(define (eval-linklets h)
  (for/hash ([(name v) (in-hash h)])
    (values name
            (if (linklet-directory? v)
                v
                (eval-linklet v)))))
