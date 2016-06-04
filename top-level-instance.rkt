#lang racket/base
(require "scope.rkt"
         "phase.rkt"
         "module-binding.rkt"
         "linklet.rkt")

;; Run-time support for evaluating top-level forms
(provide top-level-instance)

(define top-level-instance (make-instance 'top-level))

(set-instance-variable-value!
 top-level-instance
 'top-level-bind!
 (lambda (id mpi orig-phase phase-shift sym)
   (define phase (phase+ orig-phase phase-shift))
   (add-binding! id (make-module-binding mpi phase sym) phase)))
