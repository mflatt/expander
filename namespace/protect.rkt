#lang racket/base

;; Wrapper for provides that are protected

(provide (struct-out protected))

(struct protected (binding) #:prefab)
