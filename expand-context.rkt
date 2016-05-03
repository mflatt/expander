#lang racket/base
(require "namespace.rkt"
         "binding.rkt")

(provide (struct-out expand-context)
         current-expand-context)

(struct expand-context (scopes     ; list of scopes that should be pruned by `quote-syntax`
                        use-site-scopes ; #f or boxed list: scopes that should be pruned from binders
                        module-scopes ; list of scopes for enclosing module ot top level
                        context    ; 'expression, 'module, or 'top-level
                        phase      ; current expansion phase
                        namespace  ; namespace for modules and top-levels
                        env        ; environment for local bindings
                        only-immediate? ; #t => stop at core forms
                        post-expansion-scope  ; scope to add to every expansion; #f if none
                        ))

(define current-expand-context (make-parameter
                                (expand-context null ; scopes
                                                #f ; use-site scopes
                                                null ; module-scopes
                                                'expression
                                                0
                                                (current-namespace)
                                                empty-env
                                                #f
                                                #f)))
