#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "binding.rkt"
         "core.rkt"
         "expand.rkt")

;; ----------------------------------------

(add-core-form!
 'lambda
 (lambda (s env)
   (define m (match-syntax s '(lambda (id ...) body)))
   (define sc (new-scope))
   ;; Check formal arguments:
   (define ids (for/list ([id (in-list (m 'id))])
                 (add-scope id sc)))
   ;; Bind each argument and generate a corresponding key for the
   ;; expand-time environment:
   (define keys (for/list ([id (in-list ids)])
                  (add-local-binding! id)))
   (define body-env (for/fold ([env env]) ([key (in-list keys)])
                      (env-extend env key variable)))
   ;; Expand the function body:
   (define exp-body (expand (add-scope (m 'body) sc)
                            body-env))
   (rebuild
    s
    `(,(m 'lambda) ,ids ,exp-body))))

;; ----------------------------------------

(add-core-form!
 'let-syntax
 (lambda (s env)
   (define m (match-syntax s '(let-syntax ([trans-id trans-rhs]
                                           ...)
                                 body)))
   (define sc (new-scope))
   ;; Add the new scope to each binding identifier:
   (define trans-ids (for/list ([id (in-list (m 'trans-id))])
                       (add-scope id sc)))
   ;; Bind each left-hand identifier and generate a corresponding key
   ;; for the expand-time environment:
   (define trans-keys (for/list ([id (in-list trans-ids)])
                        (add-local-binding! id)))
   ;; Evaluate compile-time expressions:
   (define trans-vals (for/list ([rhs (in-list (m 'trans-rhs))])
                        (eval-for-syntax-binding rhs env)))
   ;; Fill expansion-time environment:
   (define body-env (for/fold ([env env]) ([key (in-list trans-keys)]
                                           [val (in-list trans-vals)])
                      (env-extend env key val)))
   ;; Expand body
   (expand (add-scope (m 'body) sc) body-env)))

;; ----------------------------------------

(add-core-form!
 '#%app
 (lambda (s env)
   (define m (match-syntax s '(#%app rator rand ...)))
   (rebuild
    s
    (list* (m '#%app)
           (expand (m 'rator) env)
           (for/list ([rand (in-list (m 'rand))])
             (expand rand env))))))

(add-core-form!
 'quote
 (lambda (s env)
   (match-syntax s '(quote datum))
   s))

(add-core-form!
 'quote-syntax
 (lambda (s env)
   (match-syntax s '(quote-syntax datum))
   s))
