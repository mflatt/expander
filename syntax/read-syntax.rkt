#lang racket/base
(require "../host/host-syntax-to-syntax.rkt"
         (only-in "../host/syntax.rkt"
                  [read-syntax host:read-syntax]
                  [read-syntax/recursive host:read-syntax/recursive]))

(provide read-syntax
         read-syntax/recursive)

(define (read-syntax src in)
  (host-syntax->syntax (host:read-syntax src in)))

(define (read-syntax/recursive src in start readtable graph?)
  (host-syntax->syntax (host:read-syntax/recursive src in start readtable graph?)))
