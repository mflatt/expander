#lang racket/base
(require "set.rkt"
         "syntax.rkt"
         "core.rkt"
         "namespace.rkt"
         "binding.rkt"
         "core-primitives.rkt"
         "module-path.rkt"
         (only-in racket/base
                  [dynamic-require base:dynamic-require]))

;; The '#%kernel module combines '#%core, '#%runtime, and '#%main

(provide declare-kernel-module!
         copy-racket-module!
         declare-hash-based-module!)

(define (declare-kernel-module! ns #:eval eval #:main-ids main-ids)
  (copy-racket-module! '#%kernel
                       #:to '#%runtime
                       #:skip (set-union primitive-ids
                                         main-ids)
                       #:namespace ns
                       #:primitive? #t)
  (namespace-module-visit! ns core-module-name 0)
  (eval (datum->syntax
         core-stx
         '(module #%kernel '#%core
           (#%require '#%runtime
                      '#%main)
           (#%provide (all-from '#%core)
                      (all-from '#%runtime)
                      (all-from '#%main))
           (#%declare #:cross-phase-persistent)))
        ns))

 
(define (copy-racket-module! name
                             #:to [to-name name]
                             #:namespace ns
                             #:skip [skip-syms (seteq)]
                             #:alts [alts #hasheq()]
                             #:primitive? [primitive? #f])
  (define mod-name `',name)
  (define-values (vars transes) (module->exports mod-name))
  (define syms (for/list ([sym (in-list (map car (cdr (assv 0 vars))))]
                          #:unless (set-member? skip-syms sym))
                 sym))
  (define ht
    (for/hash ([sym (in-list syms)])
      (values sym (or (hash-ref alts sym #f)
                      (base:dynamic-require mod-name sym)))))
  
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
                      (namespace-set-variable! ns 0 sym val)))))))
