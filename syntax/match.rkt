#lang racket/base
(require "../common/make-match.rkt"
         "syntax.rkt"
         "scope.rkt"
         "error.rkt")

(provide match-syntax
         try-match-syntax)

(define-values (match-syntax try-match-syntax)
  (make-syntax-matchers syntax? syntax-e raise-syntax-error))
