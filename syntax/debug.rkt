#lang racket/base
(require "../common/set.rkt"
         "syntax.rkt"
         "scope.rkt"
         "fallback.rkt"
         (submod "scope.rkt" for-debug)
         "binding.rkt"
         "module-binding.rkt")

(provide syntax-debug-info)

(define (syntax-debug-info s phase all-bindings?)
  (define hts
    (for/list ([smss (in-list (fallback->list (syntax-shifted-multi-scopes s)))])
      (define init-ht (if (identifier? s)
                          (hasheq 'name (syntax-e s))
                          #hasheq()))
      (define s-scs (scope-set-at-fallback s smss phase))
      (define context (scope-set->context s-scs))
      (define context-ht (hash-set init-ht 'context context))
      (define sym (syntax-e s))
      (define bindings
        (cond
         [(identifier? s)
          (for*/list ([sc (in-set s-scs)]
                      [bindings (in-value
                                 (let ([bindings (or (hash-ref (scope-bindings sc) sym #f)
                                                     #hash())])
                                   ;; Check bulk bindings; if a symbol match is found,
                                   ;; synthesize a non-bulk binding table, as long as the
                                   ;; same set of scopes is not already mapped
                                   (for*/fold ([bindings bindings])
                                              ([bulk-at (in-list (scope-bulk-bindings sc))]
                                               [bulk (in-value (bulk-binding-at-bulk bulk-at))]
                                               [syms (in-value (bulk-binding-symbols bulk s null))]
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
                  'match? (subset? scs s-scs)
                  (if (local-binding? b)
                      'local
                      'module)
                  (if (local-binding? b)
                      (local-binding-key b)
                      (vector (module-binding-sym b)
                              (module-binding-module b)
                              (module-binding-phase b)))))]
         [else null]))
      (if (null? bindings)
          context-ht
          (hash-set context-ht 'bindings bindings))))
  (define ht (car hts))
  (if (null? (cdr hts))
      ht
      (hash-set ht 'fallbacks (cdr hts))))

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
