#lang racket/base
(require "compile-impl-id.rkt"
         "linklet.rkt")

;; Compilation generates a linklet that has an `instance` argument to
;; receive instantiation information: a namspace, its phase, etc.

(provide instance-imports
         make-instance-instance)

(define instance-imports
  `([namespace ,ns-id]
    [phase-shift ,phase-shift-id]
    [self ,self-id]
    [bulk-binding-registry ,bulk-binding-registry-id]
    [set-transformer! ,set-transformer!-id]))

(define (make-instance-instance #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:bulk-binding-registry bulk-binding-registry
                                #:set-transformer! set-transformer!)
  (define i (make-instance 'instance))
  (instance-set-variable-value! i 'namespace ns)
  (instance-set-variable-value! i 'phase-shift phase-shift)
  (instance-set-variable-value! i 'self self)
  (instance-set-variable-value! i 'bulk-binding-registry bulk-binding-registry)
  (instance-set-variable-value! i 'set-transformer! set-transformer!)
  i)
