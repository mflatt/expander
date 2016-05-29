#lang racket/base
(require "set.rkt"
         "syntax.rkt"
         "core.rkt"
         "namespace.rkt"
         "binding.rkt"
         "core-primitives.rkt"
         "module-path.rkt"
         "require+provide.rkt"
         "linklet.rkt"
         (only-in racket/base
                  [dynamic-require base:dynamic-require]))

;; The '#%kernel module combines '#%core, '#%runtime, and '#%main

(provide declare-kernel-module!
         copy-racket-module!
         declare-hash-based-module!
         declare-reexporting-module!)

(define (declare-kernel-module! ns #:eval eval #:main-ids main-ids)
  (copy-racket-module! '#%kernel
                       #:to '#%runtime
                       #:skip (set-union primitive-ids
                                         main-ids)
                       #:namespace ns
                       #:primitive? #t)
  (declare-reexporting-module! '#%kernel '(#%core #%runtime #%main)
                               #:namespace ns))
 
(define (copy-racket-module! name
                             #:to [to-name name]
                             #:namespace ns
                             #:skip [skip-syms (seteq)]
                             #:alts [alts #hasheq()]
                             #:primitive? [primitive? #f])
  (define mod-name `',name)
  (define inst (lookup-primitive-instance name))
  (define ht (for/hash ([sym (in-list (instance-variable-names inst))]
                        #:unless (set-member? skip-syms sym))
               (values sym
                       (or (hash-ref alts sym #f)
                           (instance-variable-value inst sym)))))
  (declare-hash-based-module! to-name ht
                              #:namespace ns
                              #:primitive? primitive?))


(define (declare-hash-based-module! name ht
                                    #:namespace ns
                                    #:primitive? [primitive? #f])
  (define mpi (module-path-index-join (list 'quote name) #f))
  (declare-module!
   ns
   (make-module #:cross-phase-persistent? #t
                #:primitive? primitive?
                mpi
                #hasheqv()
                (hasheqv 0 (for/hash ([sym (in-hash-keys ht)])
                             (values sym
                                     (make-module-binding mpi 0 sym))))
                0 0
                (lambda (ns phase-shift phase-level self bulk-binding-registry)
                  (when (= 0 phase-level)
                    (for ([(sym val) (in-hash ht)])
                      (namespace-set-variable! ns 0 sym val)))))
   (module-path-index-resolve mpi)))

(define (declare-reexporting-module! name require-names
                                     #:reexport? [reexport? #t]
                                     #:namespace ns)
  (define mpi (module-path-index-join (list 'quote name) #f))
  (define require-mpis (for/list ([require-name (in-list require-names)])
                         (module-path-index-join (list 'quote require-name) #f)))
  (declare-module!
   ns
   (make-module #:cross-phase-persistent? #t
                mpi
                (hasheqv 0 require-mpis)
                (if reexport?
                    (hasheqv 0
                             (for*/hash ([require-mpi (in-list require-mpis)]
                                         [m (in-value (namespace->module
                                                       ns
                                                       (module-path-index-resolve require-mpi)))]
                                         [(sym binding) (in-hash
                                                         (hash-ref
                                                          (shift-provides-module-path-index
                                                           (module-provides m)
                                                           (module-self m)
                                                           require-mpi)
                                                          0))])
                               (values sym binding)))
                    #hasheqv())
                0 0
                void)
   (module-path-index-resolve mpi)))
