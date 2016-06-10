#lang racket/base
(require "namespace.rkt"
         "../common/module-path.rkt"
         "../common/phase.rkt"
         "../common/contract.rkt")

(provide namespace-attach-module
         namespace-attach-module-declaration)

(define (namespace-attach-module src-namespace	 	 	 	 
                                 mod-path
                                 [dest-namespace (current-namespace)])
  (do-attach-module 'namespace-attach-module
                    src-namespace mod-path dest-namespace
                    #:attach-instances? #t))

(define (namespace-attach-module-declaration src-namespace	 	 	 	 
                                             mod-path
                                             [dest-namespace (current-namespace)])
  (do-attach-module 'namespace-attach-module-declaration
                    src-namespace mod-path dest-namespace
                    #:attach-instances? #f))

(define (do-attach-module who
                          src-namespace mod-path dest-namespace
                          #:attach-instances? [attach-instances? #f])
  (check who namespace? src-namespace)
  (check who module-path? mod-path)
  (check who namespace? dest-namespace)

  (define phase (namespace-phase src-namespace))
  (unless (eqv? phase (namespace-phase dest-namespace))
    (raise-arguments-error who
                           "source and destination namespace phases do not match"
                           "source phase" phase
                           "destination phase" (namespace-phase dest-namespace)))
  
  (define todo (make-hasheq)) ; module name -> phase -> namespace
  
  (define initial-phase phase) ; phase to attach instances

  (let loop ([mpi (module-path-index-join mod-path #f)]
             [phase phase])
    (define mod-name (parameterize ([current-namespace src-namespace])
                       (module-path-index-resolve mpi)))
    
    (unless (hash-ref (hash-ref todo mod-name #hasheqv()) phase #f)
      (define m (namespace->module src-namespace mod-name))
      (unless m
        (raise-arguments-error who
                               "module not declared (in the source namespace)"
                               "module name" mod-name))

      (define already-m (namespace->module dest-namespace mod-name))
      (when (and already-m (not (eq? already-m m)))
        (raise-arguments-error who
                               "a different declaration is already in the destination namespace"
                               "module name" mod-name))

      (define-values (m-ns already?)
        (cond
         [attach-instances?
          (define m-ns (namespace->module-namespace src-namespace mod-name phase))
          (unless m-ns
            (raise-arguments-error who
                                   "module not instantiated (in the source namespace)"
                                   "module name" mod-name))
          
          (define already-m-ns (and already-m
                                    (namespace->module-namespace dest-namespace mod-name phase)))
          (when (and already-m-ns
                     (not (namespace-same-instance? m-ns already-m-ns)))
            (raise-arguments-error who
                                   "a different instance is already in the destination namespace"
                                   "module name" mod-name))

          (values m-ns (and already-m-ns #t))]
         [else
          (values #t (and already-m #t))]))

      (hash-update! todo mod-name (lambda (ht) (hash-set ht phase m-ns)) #hasheqv())
      
      (unless already?
        (for* ([(req-phase reqs) (in-hash (module-requires m))]
               [req (in-list reqs)])
          (loop (module-path-index-shift req
                                         (module-self m)
                                         mpi)
                (phase+ phase req-phase))))))

  (parameterize ([current-namespace dest-namespace]) ; for resolver notifications
    (for* ([(mod-name phases) (in-hash todo)]
           [(phase ns) (in-hash phases)])
      (define m (namespace->module src-namespace mod-name))
      (declare-module! dest-namespace m mod-name)
      (when attach-instances?
        (define m-ns (namespace->module-namespace src-namespace mod-name phase))
        (namespace->module-namespace dest-namespace mod-name phase #:install!-namespace m-ns)))))
