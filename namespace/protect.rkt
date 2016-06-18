#lang racket/base

(provide (struct-out protected))

;; Wrapper for provides that are protected
(struct protected (binding) #:prefab)
