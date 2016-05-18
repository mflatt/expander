#lang racket/base
(require "syntax.rkt"
         "namespace.rkt"
         "binding.rkt"
         "env.rkt"
         "free-id-set.rkt")

(provide (struct-out expand-context)
         make-expand-context
         current-expand-context
         
         as-expression-context
         as-tail-context
         as-named-context)

(struct expand-context (scopes     ; list of scopes that should be pruned by `quote-syntax`
                        use-site-scopes ; #f or boxed list: scopes that should be pruned from binders
                        module-scopes ; list of scopes for enclosing module or top level
                        all-scopes-stx ; all scopes from enclosing binding context; for `syntax-local-get-shadower`
                        context    ; 'expression, 'module, or 'top-level
                        frame-id   ; #f or a gensym to identify a binding frame
                        phase      ; current expansion phase
                        namespace  ; namespace for modules and top-levels
                        env        ; environment for local bindings
                        only-immediate? ; #t => stop at core forms
                        post-expansion-scope  ; scope to add to every expansion; #f if none
                        module-begin-k ; expander for `#%module-begin` in a 'module-begin context
                        need-eventually-defined ; phase(>=1) -> variables expanded before binding
                        stops      ; free-id-set
                        current-introduction-scopes ; scope for current macro expansion
                        declared-submodule-names ; hash table (mutable if non-empty): symbol -> 'module or 'module*
                        lifts      ; #f or lift-context, which contains a list of lifteds
                        lift-envs  ; list of box of env for lifts to locals
                        module-lifts ; lifted modules
                        lifts-to-module ; more lifts: requires, provides, etc.
                        requires+provides ; enclosing module's requires and provides during `provide`
                        name       ; identifier to name the expression
                        ))

(define (make-expand-context ns)
  (expand-context null ; scopes
                  #f ; use-site scopes
                  (list (namespace-scope ns)) ; module-scopes
                  empty-syntax
                  'top-level
                  #f   ; frame-id
                  (namespace-phase ns)
                  ns
                  empty-env
                  #f   ; only-immediate?
                  #f   ; post-expansion-scope
                  #f   ; module-begin-k
                  #f   ; need-eventually-defined
                  empty-free-id-set
                  null ; current-introduction-scopes
                  #hasheq() ; declared-submodule-names
                  #f   ; lifts
                  '()  ; lift-envs
                  #f   ; module-lifts
                  #f   ; lifts-for-module
                  #f   ; requires+provides
                  #f)) ; name


(define current-expand-context (make-parameter #f))

;; ----------------------------------------

;; Adjusts `ctx` to make it suitable for a subexpression of the
;; current context
(define (as-expression-context ctx)
  (cond
   [(and (eq? 'expression (expand-context-context ctx))
         (not (expand-context-name ctx)))
    ctx]
   [else (struct-copy expand-context ctx
                      [context 'expression]
                      [name #f]
                      [use-site-scopes #f]
                      [frame-id #f])]))

;; Adjusts `ctx` (which should be an expression context) to make it
;; suitable for a subexpression in tail position
(define (as-tail-context ctx #:wrt wrt-ctx)
  (cond
   [(expand-context-name wrt-ctx)
    (struct-copy expand-context ctx
                 [name (expand-context-name wrt-ctx)])]
   [else ctx]))

;; Adjust `ctx` to make it suitable for a context in the right-hand
;; side of a definition of `ids`
(define (as-named-context ctx ids)
  (cond
   [(and (pair? ids) (null? (cdr ids)))
    (struct-copy expand-context ctx
                 [name (car ids)])]
   [else ctx]))
