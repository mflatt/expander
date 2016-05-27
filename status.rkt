#lang racket/base

(provide log-status)

(define (log-status fmt . args)
  (apply fprintf (current-error-port) (string-append fmt "\n") args))
