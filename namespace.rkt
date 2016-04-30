#lang racket/base

(provide make-empty-namespace
         current-namespace
         namespace->module-namespace
         namespace-set-variable!
         namespace-set-transformer!
         namespace-get-variable
         namespace-get-transformer)

(struct namespace (phases modules))
(struct definitions (variables transformers))

(define current-namespace (make-parameter (namespace
                                           (make-hasheqv)
                                           (make-hasheq))))

(define (namespace->module-namespace ns name)
  (define m (hash-ref (namespace-modules ns) name #f))
  (or m
      (let ([m (namespace (make-hasheqv) (namespace-modules ns))])
        (hash-set! (namespace-modules ns) name m)
        m)))

(define (namespace->definitions ns phase)
  (define d (hash-ref (namespace-phases ns) phase #f))
  (or d
      (let ([d (definitions (make-hasheq) (make-hasheq))])
        (hash-set! (namespace-phases ns) phase d)
        d)))

(define (namespace-set-variable! ns phase name val)
  (define d (namespace->definitions ns phase))
  (hash-set! (definitions-variables d) name val))

(define (namespace-set-transformer! ns phase name val)
  (define d (namespace->definitions ns phase))
  (hash-set! (definitions-transformers d) name val))

(define (namespace-get-variable ns phase name fail-k)
  (define d (namespace->definitions ns phase))
  (hash-ref (definitions-variables d) name fail-k))

(define (namespace-get-transformer ns phase name fail-k)
  (define d (namespace->definitions ns phase))
  (hash-ref (definitions-transformers d) name fail-k))

