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
                                 [bindings (in-value
                                            (let ([bindings (or (hash-ref (scope-bindings sc) sym #f)
                                                                #hash())])
                                              ;; Check bulk bindings; if a symbol match is found,
                                              ;; synthesize a non-bulk binding table, as long as the
                                              ;; same set of scopes is not already mapped
                                              (for*/fold ([bindings bindings])
                                                         ([bulk-at (in-list (scope-bulk-bindings sc))]
                                                          [bulk (in-value (bulk-binding-at-bulk bulk-at))]
                                                          [syms (in-value
                                                                 (bulk-binding-symbols bulk s
                                                                                       #f
                                                                                       null))]
                                                          [b-info (in-value (hash-ref syms sym #f))]
                                                          #:when (and b-info
                                                                      (not (hash-ref bindings (bulk-binding-at-scopes bulk-at) #f))))
                                                (hash-set bindings
                                                          (bulk-binding-at-scopes bulk-at)
                                                          ((bulk-binding-create bulk) bulk b-info sym)))))]
                                 [(scs b) (in-hash bindings)]
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
     (if (representative-scope? sc)
         (vector (scope-id sc)
                 (scope-kind sc)
                 (multi-scope-name (representative-scope-owner sc)))
         (vector (scope-id sc)
                 (scope-kind sc))))
   <
   #:key (lambda (v) (vector-ref v 0))))
