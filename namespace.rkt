#lang racket/base

(provide namespace-set-variable!
         namespace-set-transformer!
         namespace-get-variable
         namespace-get-transformer
         
         current-namespace)

(struct namespace (variables      ; sym -> val
                   transformers)) ; sym -> val

(define (make-empty-namespace)
  (namespace (make-hasheq) (make-hasheq)))

(define current-namespace (make-parameter (make-empty-namespace)))

(define (namespace-set-variable! ns name val)
  (hash-set! (namespace-variables ns) name val))

(define (namespace-set-transformer! ns name val)
  (hash-set! (namespace-transformers ns) name val))

(define (namespace-get-variable ns name fail-k)
  (hash-ref (namespace-variables ns) name fail-k))

(define (namespace-get-transformer ns name fail-k)
  (hash-ref (namespace-transformers ns) name fail-k))
