#lang racket/base
(require "namespace.rkt"
         "binding.rkt"
         "free-id-set.rkt")

(provide (struct-out expand-context)
         make-expand-context
         current-expand-context)

(struct expand-context (scopes     ; list of scopes that should be pruned by `quote-syntax`
                        use-site-scopes ; #f or boxed list: scopes that should be pruned from binders
                        module-scopes ; list of scopes for enclosing module or top level
                        context    ; 'expression, 'module, or 'top-level
                        phase      ; current expansion phase
                        namespace  ; namespace for modules and top-levels
                        env        ; environment for local bindings
                        only-immediate? ; #t => stop at core forms
                        post-expansion-scope  ; scope to add to every expansion; #f if none
                        module-begin-k ; expander for `#%module-begin` in a 'module-begin context
                        need-eventually-defined ; phase(>=1) -> variables expanded before binding
                        stops      ; free-id-set
                        current-introduction-scopes ; scope for current macro expansion
                        ))

(define (make-expand-context ns)
  (expand-context null ; scopes
                  #f ; use-site scopes
                  (list (namespace-scope ns)) ; module-scopes
                  'top-level
                  0
                  ns
                  empty-env
                  #f   ; only-immediate?
                  #f   ; post-expansion-scope
                  #f   ; module-begin-k
                  #f   ; need-eventually-defined
                  empty-free-id-set
                  null)); current-introduction-scopes

(define current-expand-context (make-parameter #f))
