#lang racket/base

(provide (struct-out provided)
         provided-as-binding
         provided-as-protected?
         provided-as-transformer?)

;; Wrapper for provides that are protected or syntax
(struct provided (binding protected? syntax?) #:prefab)

(define (provided-as-binding v)
  (if (provided? v) (provided-binding v) v))
(define (provided-as-protected? v)
  (and (provided? v) (provided-protected? v)))
(define (provided-as-transformer? v)
  (and (provided? v) (provided-syntax? v)))
