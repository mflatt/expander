#lang racket/base
(require "scope.rkt"
         "core.rkt"
         "match.rkt"
         "syntax-error.rkt"
         "module-binding.rkt"
         "namespace.rkt"
         "require+provide.rkt"
         "expand.rkt"
         "expand-context.rkt"
         "expand-require.rkt"
         "expand-def-id.rkt"
         "expand-bind-top.rkt")

(add-core-form!
 'define-values
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (raise-syntax-error #f "not allowed in an expression position" s))
   (define m (match-syntax s '(define-values (id ...) rhs)))
   (define ids (as-expand-time-top-level-bindings (m 'id) ctx))
   (define exp-rhs (expand (m 'rhs) (as-named-context ctx ids)))
   (rebuild
    s
    `(,(m 'define-values) ,ids ,exp-rhs))))

(add-core-form!
 'define-syntaxes
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (raise-syntax-error #f "not allowed in an expression position" s))
   (define m (match-syntax s '(define-syntaxes (id ...) rhs)))
   (define ids (as-expand-time-top-level-bindings (m 'id) ctx))
   (define exp-rhs (expand-transformer (m 'rhs) (as-named-context ctx ids)))
   (rebuild
    s
    `(,(m 'define-syntaxes) ,ids ,exp-rhs))))

(add-core-form!
 'begin-for-syntax
 (lambda (s ctx)
   (raise-syntax-error #f "not allowed in an expression position" s)))

(add-core-form!
 '#%require
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (raise-syntax-error #f "allowed only in a module or the top level" s))
   (define m (match-syntax s '(#%require req ...)))
   (define sc (new-scope 'macro)) ; to hide bindings
   ;; Check the `#%require` form syntax and trigger compile-time
   ;; instanations
   (parse-and-perform-requires! (for/list ([req (in-list (m 'req))])
                                  (add-scope req sc))
                                s
                                #f ; no enclosing module
                                (expand-context-namespace ctx)
                                (expand-context-phase ctx)
                                (make-requires+provides #f))
   ;; Nothing to expand
   s))

(add-core-form!
 '#%provide
 (lambda (s ctx)
   (raise-syntax-error #f "not allowed outside of a module body" s)))
