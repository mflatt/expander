#lang racket/base
(require "provided.rkt")

(provide provides->api-provides
         variables->api-nonprovides)

(define (provides->api-provides provides)
  (define (extract ok?)
    (define result-l
      (for*/list ([(phase at-phase) (in-hash provides)]
                  [l (in-value
                      (for/list ([(sym b/p) (in-hash at-phase)]
                                 #:when (ok? b/p))
                        sym))]
                  #:unless (null? l))
        (cons phase (sort l symbol<?))))
    (sort result-l < #:key car))
  (values (extract (lambda (b/p) (not (provided-as-transformer? b/p))))
          (extract provided-as-transformer?)))


(define (variables->api-nonprovides provides all-vars)
  ;; Filter provideded from list of all variables
  (define result-l
    (for/list ([(phase vars) (in-hash all-vars)]
               #:when #t
               [l (in-value
                   (let ([syms (hash-ref provides phase #hasheq())])
                     (for/list ([var-sym (in-list vars)]
                                #:unless (hash-ref syms var-sym #f))
                       var-sym)))]
               #:unless (null? l))
      (cons phase (sort l symbol<?))))
  (sort result-l < #:key car))
