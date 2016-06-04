#lang racket/base
(require "scope.rkt"
         "core.rkt"
         "match.rkt"
         "module-binding.rkt"
         "namespace.rkt"
         "require+provide.rkt"
         "expand.rkt"
         "expand-context.rkt"
         "expand-require.rkt")

(add-core-form!
 'define-values
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (error "not allowed in an expression position:" s))
   (define m (match-syntax s '(define-values (id ...) rhs)))
   (define ids (for/list ([id (m 'id)])
                 (define new-id
                   (add-scope id (root-expand-context-top-level-bind-scope ctx)))
                 (add-binding! new-id
                               (make-module-binding (namespace-mpi (expand-context-namespace ctx))
                                                    (expand-context-phase ctx)
                                                    ;; FIXME:
                                                    (syntax-e id))
                               (expand-context-phase ctx))
                 new-id))
   (define exp-rhs (expand (m 'rhs)
                           (as-named-context ctx ids)))
   (rebuild
    s
    `(,(m 'define-values) ,ids ,exp-rhs))))

(add-core-form!
 'define-syntaxes
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))

(add-core-form!
 'begin-for-syntax
 (lambda (s ctx)
   (error "not yet supported here:" s)))

(add-core-form!
 '#%require
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (error "allowed only in a module or the top level:" s))
   (define m (match-syntax s '(#%require req ...)))
   (define sc (new-scope)) ; to hide bindings
   ;; Check the `#%require` form syntax and trigger compile-time
   ;; instanations
   (parse-and-perform-requires! (for ([req (in-list (m 'req))])
                                  (add-scope req sc))
                                #f ; no enclosing module
                                (expand-context-namespace ctx)
                                (expand-context-phase ctx)
                                (make-requires+provides #f))
   ;; Nothing to expand
   s))

(add-core-form!
 '#%provide
 (lambda (s ctx)
   (error "not allowed outside of a module body:" s)))
