#lang racket/base
(require "linklet.rkt"
         (prefix-in host: '#%linklet)
         "linklet-operation.rkt")

;; Run this module before "../host/linklet.rkt" to substitute the
;; implementation in "linklet.rkt"

(define bootstrap-linklet-instance
  (host:get-primitive-instance '#%bootstrap-linklet #t)) ; #t => create

(define-syntax-rule (bounce id ...)
  (begin
    (host:instance-set-variable-value! bootstrap-linklet-instance 'id id)
    ...))

(linklet-operations=> bounce)
