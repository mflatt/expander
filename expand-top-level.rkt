#lang racket/unit
(require "expand-sig.rkt")

(import expand^)
(export)

(add-core-form!
 'define-values
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))

(add-core-form!
 'define-syntaxes
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))

(add-core-form!
 '#%require
 (lambda (s ctx)
   (error "not yet supported here:" s)))
