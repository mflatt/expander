#lang racket/base
(require "reserved-symbol.rkt"
         "../host/linklet.rkt")

;; Compilation generates a linklet that has an `instance` argument to
;; receive instantiation information: a namspace, its phase, etc.

(provide instance-imports
         make-instance-instance)

(define instance-imports
  `([namespace ,ns-id]
    [phase-shift ,phase-shift-id]
    [self ,self-id]
    [bulk-binding-registry ,bulk-binding-registry-id]
    [inspector ,inspector-id]
    [set-transformer! ,set-transformer!-id]))

(define (make-instance-instance #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:bulk-binding-registry bulk-binding-registry
                                #:inspector inspector
                                #:set-transformer! set-transformer!
                                #:record-root-context! [record-root-context! #f])
  (define i (make-instance 'instance))
  (instance-set-variable-value! i 'namespace ns)
  (instance-set-variable-value! i 'phase-shift phase-shift)
  (instance-set-variable-value! i 'self self)
  (instance-set-variable-value! i 'bulk-binding-registry bulk-binding-registry)
  (instance-set-variable-value! i 'inspector inspector)
  (instance-set-variable-value! i 'set-transformer! set-transformer!)
  (when record-root-context!
    (instance-set-variable-value! i 'record-root-context! record-root-context!))
  i)
