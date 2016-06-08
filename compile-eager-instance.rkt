#lang racket/base
(require "compile-impl-id.rkt"
         "linklet.rkt")

;; Compilation generate a linklet that has an `instance` argument to
;; receive instantiation information: a namspace, its phase, etc.

(provide eager-instance-imports
         make-eager-instance-instance)

(define eager-instance-imports
  `([dest-phase ,dest-phase-id]
    [self ,self-id]
    [bulk-binding-registry ,bulk-binding-registry-id]))

(define (make-eager-instance-instance #:dest-phase dest-phase
                                      #:self self 
                                      #:bulk-binding-registry bulk-binding-registry)
  (define i (make-instance 'instance))
  (instance-set-variable-value! i 'dest-phase dest-phase)
  (instance-set-variable-value! i 'self self)
  (instance-set-variable-value! i 'bulk-binding-registry bulk-binding-registry)
  i)
