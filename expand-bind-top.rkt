#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "root-expand-context.rkt"
         "expand-def-id.rkt"
         "use-site.rkt")

;; When compiling `(define-values (x) ...)` at the top level, we'd
;; like to bind `x` so that a reference in the "..." will point back
;; to the definition, as opposed to being whatever `x` was before.
;; (The top level is hopeless, but this bit of early binding helps.)
;; We don't want that binding to take effect outside of evaluation,
;; however; the permanent binding should happen when the
;; `define-values` for is evaluated. So, we use a distinct scope that
;; effectively hides the binding from tasks other than expansion.
;;
;; See also "expand-def-id.rkt".

(provide as-expand-time-top-level-bindings)

(define (as-expand-time-top-level-bindings ids ctx)
  (define top-level-bind-scope (root-expand-context-top-level-bind-scope ctx))
  (define tl-ids
    (for/list ([id (in-list ids)])
      (add-scope (remove-use-site-scopes id ctx)
                 top-level-bind-scope)))
  (select-defined-syms-and-bind!/ctx tl-ids ctx)
  tl-ids)
