#lang racket/base

(provide log-status)

(define stderr (current-error-port))

(define (log-status fmt . args)
  (apply fprintf stderr (string-append fmt "\n") args))
