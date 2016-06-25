#lang racket/base
(require "linklet.rkt"
         (prefix-in host: '#%linklet))

;; Run this module before "../host/linklet.rkt" to substitute the
;; implementation in "linklet.rkt"

(define bootstrap-linklet-instance
  (host:get-primitive-instance '#%bootstrap-linklet #t)) ; #t => create

(define-syntax-rule (bounce id ...)
  (begin
    (host:instance-set-variable-value! bootstrap-linklet-instance 'id id)
    ...))

(bounce linklet?
        compile-linklet
        instantiate-linklet
        
        linklet-import-variables
        linklet-export-variables

        instance?
        make-instance
        instance-name
        instance-variable-names
        instance-variable-value
        instance-set-variable-value!
        instance-unset-variable!

        get-primitive-instance

        linklet-directory?
        hash->linklet-directory
        linklet-directory->hash

        linklet-bundle?
        hash->linklet-bundle
        linklet-bundle->hash
        
        variable-reference?
        variable-reference->instance
        variable-reference-constant?)
