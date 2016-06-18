#lang racket/base
(require "syntax.rkt")

(provide syntax-set-inspector)

;; When for deserialized syntax literals to associate the
;; declaration-time inspector with each syntax object
(define (syntax-set-inspector s insp)
  (syntax-map s
              (lambda (tail? d) d)
              (lambda (s d)
                (struct-copy syntax s
                             [content d]
                             [inspector (or (syntax-inspector s)
                                            insp)]))
              syntax-content))
