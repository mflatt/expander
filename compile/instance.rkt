#lang racket/base
(require "reserved-symbol.rkt"
         "../host/linklet.rkt")

;; Compilation generates a linklet that has an `instance` argument to
;; receive instantiation information: a namspace, its phase, etc.

(provide instance-imports
         make-instance-instance
         make-module-body-instance-instance)

(define instance-imports
  `(,ns-id
    ,phase-shift-id
    ,self-id
    ,inspector-id
    ,set-transformer!-id))

(define (make-instance-instance #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:bulk-binding-registry bulk-binding-registry
                                #:inspector inspector
                                #:set-transformer! set-transformer!)
  (define i (make-instance 'instance))
  (instance-set-variable-value! i ns-id ns)
  (instance-set-variable-value! i phase-shift-id phase-shift)
  (instance-set-variable-value! i self-id self)
  (instance-set-variable-value! i inspector-id inspector)
  (instance-set-variable-value! i set-transformer!-id set-transformer!)
  i)

(define (make-module-body-instance-instance #:set-transformer! set-transformer!)
  (define i (make-instance 'instance))
  (instance-set-variable-value! i set-transformer!-id set-transformer!)
  i)
