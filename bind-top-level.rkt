#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "namespace.rkt"
         "root-expand-context.rkt"
         "expand-context.rkt"
         "expand-def-id.rkt"
         "use-site.rkt")

(provide as-top-level-bindings)

(define (as-top-level-bindings ids ctx)
  (define top-level-bind-scope (root-expand-context-top-level-bind-scope ctx))
  (define tl-ids
    (for/list ([id (in-list ids)])
      (add-scope (remove-use-site-scopes id ctx)
                 top-level-bind-scope)))
  (select-defined-syms-and-bind! tl-ids (root-expand-context-defined-syms ctx)
                                 (namespace-mpi (expand-context-namespace ctx))
                                 (expand-context-phase ctx)
                                 (root-expand-context-all-scopes-stx ctx)
                                 #:frame-id (root-expand-context-frame-id ctx)
                                 #:top-level-bind-scope top-level-bind-scope)
  tl-ids)
