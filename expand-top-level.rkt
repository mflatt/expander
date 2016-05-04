#lang racket/base
(require "core.rkt"
         "expand.rkt")

(add-core-form!
 'define-values
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))

(add-core-form!
 'define-syntaxes
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))
