#lang racket/base
(require "scope.rkt"
         "phase.rkt"
         "namespace.rkt"
         "root-expand-context.rkt"
         "module-binding.rkt"
         "linklet.rkt"
         "expand-require.rkt"
         "require+provide.rkt")

;; Run-time support for evaluating top-level forms
(provide top-level-instance)

(define top-level-instance (make-instance 'top-level))

(set-instance-variable-value!
 top-level-instance
 'top-level-bind!
 (lambda (id mpi orig-phase phase-shift sym)
   (define phase (phase+ orig-phase phase-shift))
   (add-binding! id (make-module-binding mpi phase sym) phase)))

(set-instance-variable-value!
 top-level-instance
 'top-level-require!
 (lambda (stx ns)
   (define reqs (cdr (syntax-e
                      (add-scopes stx (root-expand-context-module-scopes
                                       (namespace-root-expand-ctx
                                        ns))))))
   (parse-and-perform-requires! #:run? #t
                                reqs
                                #f ; no enclosing module
                                ns
                                (namespace-phase ns)
                                (make-requires+provides #f))))
