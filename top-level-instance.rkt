#lang racket/base
(require "syntax-to-list.rkt"
         "scope.rkt"
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

(instance-set-variable-value!
 top-level-instance
 'top-level-bind!
 (lambda (id mpi orig-phase phase-shift sym)
   (define phase (phase+ orig-phase phase-shift))
   (add-binding! id (make-module-binding mpi phase sym) phase)))

(instance-set-variable-value!
 top-level-instance
 'top-level-require!
 (lambda (stx ns)
   (define reqs (cdr (syntax->list
                      (add-scopes stx (root-expand-context-module-scopes
                                       (namespace-root-expand-ctx
                                        ns))))))
   (parse-and-perform-requires! #:run? #t
                                reqs
                                #f ; no syntax errors should happen
                                #f ; no enclosing module
                                ns
                                (namespace-phase ns)
                                (make-requires+provides #f))))
