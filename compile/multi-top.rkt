#lang racket/base
(require "compiled-in-memory.rkt"
         "../host/linklet.rkt")

(provide compiled-tops->compiled-top
         compiled-top->compiled-tops)

;; Encode a sequence of compiled top-level forms by creating a linklet
;; directory using labels |0|, |1|, etc., to map to the given linklet
;; directories. Keep all the existing compile-in-memory records as
;; "pre" record, too.
(define (compiled-tops->compiled-top cims)
  (compiled-in-memory (hash->linklet-directory
                       (for/hash ([cim (in-list cims)]
                                  [i (in-naturals)])
                         (values (string->symbol (number->string i))
                                 (compiled-in-memory-linklet-directory cim))))
                      0
                      0
                      #hasheqv()
                      #()
                      #()
                      cims
                      null))

;; Decode a sequence of compiled top-level forms by unpacking the
;; linklet directory into a list of linklet directories
(define (compiled-top->compiled-tops ld)
  (define ht (linklet-directory->hash ld))
  (for/list ([i (in-range (hash-count ht))])
    (hash-ref ht (string->number (number->string i)))))
