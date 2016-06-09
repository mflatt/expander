#lang racket/base
(require "host-syntax-to-syntax.rkt"
         (only-in racket/base [read-syntax host:read-syntax]))

(provide read-syntax
         original-property-sym)

(define (read-syntax src [i (current-input-port)])
  (host-syntax->syntax (host:read-syntax src i)))
