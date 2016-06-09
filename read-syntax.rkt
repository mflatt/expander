#lang racket/base
(require "host-syntax-to-syntax.rkt"
         (only-in racket/base
                  [read-syntax host:read-syntax]
                  [read-syntax/recursive host:read-syntax/recursive]))

(provide read-syntax
         read-syntax/recursive
         original-property-sym)

(define (read-syntax src in)
  (host-syntax->syntax (host:read-syntax src in)))

(define (read-syntax/recursive src in start readtable graph?)
  (host-syntax->syntax (host:read-syntax/recursive src in start readtable graph?)))
