#lang racket/base
(require "reserved-symbol.rkt"
         "../host/linklet.rkt"
         "namespace-scope.rkt")

;; Compilation of top-level forms generates a link that has an
;; `eager-instance` argument to receive deserialization information: a
;; namspace, its phase, etc.

(provide eager-instance-imports
         make-eager-instance-instance)

(define eager-instance-imports
  `([namespace ,ns-id]
    [dest-phase ,dest-phase-id]
    [self ,self-id]
    [bulk-binding-registry ,bulk-binding-registry-id]
    [inspector ,inspector-id]
    swap-top-level-scopes))

(define (make-eager-instance-instance #:namespace ns
                                      #:dest-phase dest-phase
                                      #:self self 
                                      #:bulk-binding-registry bulk-binding-registry
                                      #:inspector inspector)
  (define i (make-instance 'instance))
  (instance-set-variable-value! i 'namespace ns)
  (instance-set-variable-value! i 'dest-phase dest-phase)
  (instance-set-variable-value! i 'self self)
  (instance-set-variable-value! i 'bulk-binding-registry bulk-binding-registry)
  (instance-set-variable-value! i 'inspector inspector)
  (instance-set-variable-value! i 'swap-top-level-scopes swap-top-level-scopes)
  i)
