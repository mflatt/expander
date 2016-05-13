#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         (submod "scope.rkt" for-debug)
         "binding.rkt")

(provide syntax-debug-info)

(define (syntax-debug-info s phase all-bindings?)
  (define init-ht (if (identifier? s)
                      (hasheq 'name (syntax-e s))
                      #hasheq()))
  (define s-scs (syntax-scope-set s phase))
  (define context (scope-set->context s-scs))
  (define context-ht (hash-set init-ht 'context context))
  (define bindings (cond
                    [(identifier? s)
                     (for*/list ([sc (in-set (syntax-scope-set s phase))]
                                 [(scs b) (in-hash (hash-ref (scope-bindings sc)
                                                             (syntax-e s)
                                                             #hash()))]
                                 #:when (or all-bindings?
                                            (subset? scs s-scs)))
                       (hash 'name (syntax-e s)
                             'context (scope-set->context scs)
                             (if (local-binding? b)
                                 'local
                                 'module)
                             (if (local-binding? b)
                                 (local-binding-key b)
                                 b)))]
                    [else null]))
  (if (null? bindings)
      context-ht
      (hash-set context-ht 'binding bindings)))
                         
(define (scope-set->context scs)
  (sort
   (for/list ([sc (in-set scs)])
     (vector (scope-id sc)
             (scope-kind sc)))
   <
   #:key (lambda (v) (vector-ref v 0))))
