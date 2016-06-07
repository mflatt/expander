#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "binding.rkt"
         "env.rkt"
         "free-id-set.rkt"
         "namespace.rkt"
         "root-expand-context.rkt")

(provide (struct-out expand-context)
         (all-from-out "root-expand-context.rkt")
         make-expand-context
         copy-root-expand-context
         current-expand-context
         
         as-expression-context
         as-tail-context
         as-named-context)


;; An `expand-context` has the rest of the information for an expansion 
(struct expand-context root-expand-context (context    ; 'expression, 'module, or 'top-level
                                            phase      ; current expansion phase; must match phase of `namespace`
                                            namespace  ; namespace for modules and evaluation
                                            env        ; environment for local bindings
                                            scopes     ; list of scopes that should be pruned by `quote-syntax`
                                            def-ctx-scopes ; #f or box of list of scopes; transformer-created def-ctxes
                                            only-immediate? ; #t => stop at core forms
                                            module-begin-k ; expander for `#%module-begin` in a 'module-begin context
                                            need-eventually-defined ; phase(>=1) -> variables expanded before binding
                                            allow-unbound? ; allow reference to unbound identifiers as variables
                                            stops      ; free-id-set
                                            current-introduction-scopes ; scopes for current macro expansion
                                            declared-submodule-names ; mutable hash table: symbol -> 'module or 'module*
                                            lifts      ; #f or lift-context, which contains a list of lifteds
                                            lift-envs  ; list of box of env for lifts to locals
                                            module-lifts ; lifted modules
                                            lifts-to-module ; more lifts: requires, provides, etc.
                                            requires+provides ; enclosing module's requires+provides during `provide`
                                            name))     ; #f or identifier to name the expression

(define (make-expand-context ns)
  (define root-ctx (namespace-root-expand-ctx ns))
  (expand-context (root-expand-context-module-scopes root-ctx)
                  (root-expand-context-post-expansion-scope root-ctx)
                  (root-expand-context-top-level-bind-scope root-ctx)
                  (root-expand-context-all-scopes-stx root-ctx)
                  (root-expand-context-use-site-scopes root-ctx)
                  (root-expand-context-defined-syms root-ctx)
                  (root-expand-context-frame-id root-ctx)
                  (root-expand-context-counter root-ctx)
                  'top-level
                  (namespace-phase ns)
                  ns
                  empty-env
                  null ; scopes
                  #f   ; def-ctx-scopes
                  #f   ; only-immediate?
                  #f   ; module-begin-k
                  #f   ; need-eventually-defined
                  #t   ; allow-unbound?
                  empty-free-id-set
                  null ; current-introduction-scopes
                  #hasheq() ; declared-submodule-names
                  #f   ; lifts
                  '()  ; lift-envs
                  #f   ; module-lifts
                  #f   ; lifts-for-module
                  #f   ; requires+provides
                  #f)) ; name

(define (copy-root-expand-context ctx root-ctx)
  (struct-copy expand-context ctx
               [module-scopes #:parent root-expand-context (root-expand-context-module-scopes root-ctx)]
               [post-expansion-scope #:parent root-expand-context (root-expand-context-post-expansion-scope root-ctx)]
               [top-level-bind-scope #:parent root-expand-context (root-expand-context-top-level-bind-scope root-ctx)]
               [all-scopes-stx #:parent root-expand-context (root-expand-context-all-scopes-stx root-ctx)]
               [use-site-scopes #:parent root-expand-context (root-expand-context-use-site-scopes root-ctx)]
               [defined-syms #:parent root-expand-context (root-expand-context-defined-syms root-ctx)]
               [frame-id #:parent root-expand-context (root-expand-context-frame-id root-ctx)]
               [counter #:parent root-expand-context (root-expand-context-counter root-ctx)]))

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
                      [use-site-scopes #:parent root-expand-context #f]
                      [frame-id #:parent root-expand-context #f])]))

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
