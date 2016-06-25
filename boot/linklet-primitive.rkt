#lang racket/base
(require "../host/linklet.rkt"
         "../common/reflect-hash.rkt")

(provide linklet-primitives)

(define linklet-primitives
  (reflect-hash linklet?
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
                variable-reference-constant?))
