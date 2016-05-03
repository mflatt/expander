#lang racket/base
(require "syntax.rkt"
         "core.rkt"
         "expand.rkt"
         ;; These modules register core forms:
         "expand-expr.rkt"
         "expand-module.rkt"
         "expand-top-level.rkt")

;; This list will need to be a lot longer...
(add-core-primitive! 'syntax-e syntax-e)
(add-core-primitive! 'datum->syntax datum->syntax)
(add-core-primitive! 'cons cons)
(add-core-primitive! 'list list)
(add-core-primitive! 'car car)
(add-core-primitive! 'cdr cdr)
(add-core-primitive! 'null? null?)
(add-core-primitive! 'values values)
(add-core-primitive! 'println println)
(add-core-primitive! 'random random)
