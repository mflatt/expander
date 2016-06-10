#lang racket/base
(require "../syntax/syntax-to-list.rkt"
         "../syntax/scope.rkt"
         "../common/phase.rkt"
         "../namespace/namespace.rkt"
         "../expand/root-expand-context.rkt"
         "../syntax/module-binding.rkt"
         "../host/linklet.rkt"
         "../expand/require.rkt"
         "../expand/require+provide.rkt")

;; Run-time support for evaluating top-level forms
(provide top-level-instance)

(define top-level-instance (make-instance 'top-level))

(instance-set-variable-value!
 top-level-instance
 'top-level-bind!
 (lambda (id mpi orig-phase phase-shift ns sym)
   (define phase (phase+ orig-phase phase-shift))
   (define b (make-module-binding mpi phase sym
                                  #:frame-id (root-expand-context-frame-id
                                              (namespace-root-expand-ctx ns))))
   (add-binding! id b phase)))

(instance-set-variable-value!
 top-level-instance
 'top-level-require!
 (lambda (stx ns)
   (define reqs (cdr (syntax->list stx)))
   (parse-and-perform-requires! #:run? #t
                                reqs
                                #f ; no syntax errors should happen
                                #f ; no enclosing module
                                ns
                                (namespace-phase ns)
                                (make-requires+provides #f))))
