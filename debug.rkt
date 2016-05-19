#lang racket/base
(require "set.rkt"
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
  (define sym (syntax-e s))
  (define bindings (cond
                    [(identifier? s)
                     (for*/list ([sc (in-set (syntax-scope-set s phase))]
                                 [(scs b) (in-hash 
                                           (or
                                            (hash-ref (or (scope-bindings sc) #hasheq()) sym #f)
                                            ;; Check bulk bindings; if a symbol match is found,
                                            ;; synthesize a non-bulk binding table
                                            (for/or ([bulk-at (in-list (scope-bulk-bindings sc))])
                                              (define bulk (bulk-binding-at-bulk bulk-at))
                                              (define syms (bulk-binding-symbols bulk s))
                                              (define b-info (hash-ref syms sym #f))
                                              (and b-info
                                                   (hasheq (bulk-binding-at-scopes bulk-at)
                                                           ((bulk-binding-create bulk) bulk b-info sym))))
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
