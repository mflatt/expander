#lang racket/base

(provide resolve-module-path)

(define (resolve-module-path p)
  ;; Assume 'name, for now
  (cadr p))
