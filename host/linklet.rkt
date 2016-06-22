#lang racket/base
(require (prefix-in host: '#%linklet))

;; We use only `get-primitive-instance` and `instance-variable-value`
;; directly, so that those are the only functions needed for
;; bootstrapping --- and generally so we can replace the linklet
;; implementation for bootstrapping. See also "../run/linklet.rkt".

(define linklet-instance (or
                          ;; As a hook for bootstrapping, check for a
                          ;; replacement of the primitive '#%linklet
                          ;; module:
                          (host:get-primitive-instance '#%bootstrap-linklet)
                          (host:get-primitive-instance '#%linklet)))

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (define id (host:instance-variable-value linklet-instance 'id))
    ...))

(bounce compile-linklet
        eval-linklet
        instantiate-linklet
        
        linklet-import-variables
        linklet-export-variables
        compiled-linklet-import-variables
        compiled-linklet-export-variables

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

(unless variable-reference-constant?
  (error "broken primitive '#%linklet instance; maybe you need to use \"bootstrap-run.rkt\""))
