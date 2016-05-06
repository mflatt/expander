#lang racket/base
(require "scope.rkt"
         "core.rkt"
         "match.rkt"
         "require+provide.rkt"
         "expand.rkt"
         "expand-context.rkt"
         "expand-require.rkt")

(add-core-form!
 'define-values
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))

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
                                (make-requires+provides))
   ;; Nothing to expand
   s))

(add-core-form!
 '#%provide
 (lambda (s ctx)
   (error "not allowed outside of a module body:" s)))
