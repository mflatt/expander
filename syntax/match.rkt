#lang racket/base
(require "../common/make-match.rkt"
         "syntax.rkt"
         "scope.rkt"
         "error.rkt")

(provide define-match)

(define-define-match define-match
  syntax? syntax-e raise-syntax-error)
