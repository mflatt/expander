#lang racket/base
(require "compile-impl-id.rkt"
         "linklet.rkt")

;; Compilation generate a linklet that has an `instance` argument to
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
  (set-instance-variable-value! i 'namespace ns)
  (set-instance-variable-value! i 'phase-shift phase-shift)
  (set-instance-variable-value! i 'self self)
  (set-instance-variable-value! i 'bulk-binding-registry bulk-binding-registry)
  (set-instance-variable-value! i 'set-transformer! set-transformer!)
  i)
