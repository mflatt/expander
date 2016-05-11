#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "namespace.rkt"
         "binding.rkt"
         "dup-check.rkt"
         "core.rkt"
         "expand-context.rkt"
         "expand.rkt")

;; ----------------------------------------

;; Common expansion for `lambda` and `case-lambda`
(define (make-lambda-expander s formals bodys ctx)
  (define sc (new-scope))
  ;; Parse and check formal arguments:
  (define ids (parse-and-flatten-formals formals sc))
  (check-no-duplicate-ids ids s)
  ;; Bind each argument and generate a corresponding key for the
  ;; expand-time environment:
  (define keys (for/list ([id (in-list ids)])
                 (add-local-binding! id)))
  (define body-env (for*/fold ([env (expand-context-env ctx)]) ([key (in-list keys)])
                     (env-extend env key variable)))
  ;; Expand the function body:
  (define body-ctx (struct-copy expand-context ctx
                                [env body-env]))
  (define exp-body (expand-body bodys sc s body-ctx))
  ;; Return formals (with new scope) and expanded body:
  (values (add-scope formals sc)
          exp-body))

(add-core-form!
 'lambda
 (lambda (s ctx)
   (define m (match-syntax s '(lambda formals body ...+)))
   (define-values (formals body)
     (make-lambda-expander s (m 'formals) (m 'body) ctx))
   (rebuild
    s
    `(,(m 'lambda) ,formals ,body))))

(add-core-form!
 'case-lambda
 (lambda (s ctx)
   (define m (match-syntax s '(case-lambda [formals body ...+] ...)))
   (define cm (match-syntax s '(case-lambda clause ...)))
   (rebuild
    s
    `(,(m 'case-lambda)
      ,@(for/list ([formals (in-list (m 'formals))]
                   [bodys (in-list (m 'body))]
                   [clause (in-list (cm 'clause))])
          (define-values (exp-formals exp-body)
            (make-lambda-expander s formals bodys ctx))
          (rebuild clause `[,exp-formals ,exp-body]))))))

(define (parse-and-flatten-formals all-formals sc)
  (let loop ([formals all-formals])
    (cond
     [(identifier? formals) (list (add-scope formals sc))]
     [(syntax? formals)
      (define p (syntax-e formals))
      (cond
       [(pair? p) (loop p)]
       [(null? p) null]
       [else (error "not an identifier:" p)])]
     [(pair? formals)
      (unless (identifier? (car formals))
        (error "not an identifier:" (car formals)))
      (cons (add-scope (car formals) sc)
            (loop (cdr formals)))]
     [(null? formals)
      null]
     [else
      (error "bad argument sequence:" all-formals)])))

;; ----------------------------------------

;; Common expansion for `let[rec]-[syntaxes+]values`
(define (make-let-values-form syntaxes? rec?)
  (lambda (s ctx)
    (define m (if syntaxes?
                  (match-syntax s '(letrec-syntaxes+values
                                    ([(trans-id ...) trans-rhs] ...)
                                    ([(val-id ...) val-rhs] ...)
                                    body ...+))
                  (match-syntax s '(let-values ([(val-id ...) val-rhs] ...)
                                    body ...+))))
   (define sc (new-scope))
   ;; Add the new scope to each binding identifier:
   (define trans-idss (for/list ([ids (in-list (m 'trans-id))])
                        (for/list ([id (in-list ids)])
                          (add-scope id sc))))
   (define val-idss (for/list ([ids (in-list (m 'val-id))])
                      (for/list ([id (in-list ids)])
                        (add-scope id sc))))
   (check-no-duplicate-ids (list trans-idss val-idss) s)
   ;; Bind each left-hand identifier and generate a corresponding key
   ;; fo the expand-time environment:
   (define trans-keyss (for/list ([ids (in-list trans-idss)])
                         (for/list ([id (in-list ids)])
                           (add-local-binding! id))))
   (define val-keyss (for/list ([ids (in-list val-idss)])
                       (for/list ([id (in-list ids)])
                         (add-local-binding! id))))
   ;; Evaluate compile-time expressions (if any):
   (define trans-valss (for/list ([rhs (in-list (m 'trans-rhs))]
                                  [ids (in-list trans-idss)])
                         (eval-for-syntaxes-binding (add-scope rhs sc) ids ctx)))
   ;; Fill expansion-time environment:
   (define rec-val-env
     (for*/fold ([env (expand-context-env ctx)]) ([keys (in-list val-keyss)]
                                                  [key (in-list keys)])
       (env-extend env key variable)))
   (define rec-env (for/fold ([env rec-val-env]) ([keys (in-list trans-keyss)]
                                                  [vals (in-list trans-valss)])
                     (for/fold ([env env]) ([key (in-list keys)]
                                            [val (in-list vals)])
                       (env-extend env key val))))
   ;; Expand right-hand sides and bodyL
   (define rec-ctx (struct-copy expand-context ctx
                                [env rec-env]))
   (define letrec-values-id
     (if syntaxes?
         (datum->syntax core-stx 'letrec-values)
         (m 'let-values)))
   (rebuild
    s
    `(,letrec-values-id ,(for/list ([ids (in-list val-idss)]
                                    [rhs (in-list (m 'val-rhs))])
                           `[,ids ,(if rec?
                                       (expand (add-scope rhs sc) rec-ctx)
                                       (expand rhs ctx))])
      ,(expand-body (m 'body) sc s rec-ctx)))))

(add-core-form!
 'let-values
 (make-let-values-form #f #f))

(add-core-form!
 'letrec-values
 (make-let-values-form #f #t))

(add-core-form!
 'letrec-syntaxes+values
 (make-let-values-form #t #t))

;; ----------------------------------------

(add-core-form!
 '#%datum
 (lambda (s ctx)
   (define m (match-syntax s '(#%datum . datum)))
   (when (keyword? (syntax-e (m 'datum)))
     (error "keyword misused as an expression:" (m 'datum)))
   (rebuild
    s
    (list (datum->syntax core-stx 'quote)
          (m 'datum)))))

(add-core-form!
 '#%app
 (lambda (s ctx)
   (define m (match-syntax s '(#%app rator rand ...)))
   (rebuild
    s
    (list* (m '#%app)
           (expand (m 'rator) ctx)
           (for/list ([rand (in-list (m 'rand))])
             (expand rand ctx))))))

(add-core-form!
 'quote
 (lambda (s ctx)
   (match-syntax s '(quote datum))
   s))

(add-core-form!
 'quote-syntax
 (lambda (s ctx)
   (match-syntax s '(quote-syntax datum))
   s))

(add-core-form!
 'if
 (lambda (s ctx)
   (define m (match-syntax s '(if tst thn els)))
   (rebuild
    s
    (list (m 'if)
          (expand (m 'tst) ctx)
          (expand (m 'thn) ctx)
          (expand (m 'els) ctx)))))

(add-core-form!
 'with-continuation-mark
 (lambda (s ctx)
   (define m (match-syntax s '(with-continuation-mark key val body)))
   (rebuild
    s
    (list (m 'with-continuation-mark)
          (expand (m 'key) ctx)
          (expand (m 'val) ctx)
          (expand (m 'body) ctx)))))

(define (make-begin)
 (lambda (s ctx)
   (define m (match-syntax s '(begin e ...+)))
   (rebuild
    s
    (cons (m 'begin)
          (for/list ([e (in-list (m 'e))])
            (expand e ctx))))))

(add-core-form!
 'begin
 (make-begin))

(add-core-form!
 'begin0
 (make-begin))

(add-core-form!
 'set!
 (lambda (s ctx)
   (define m (match-syntax s '(set! id rhs)))
   (define binding (resolve (m 'id)))
   (unless binding
     (error "no binding for assignment:" s))
   (define t (lookup binding ctx s))
   (unless (variable? t)
     (if (unbound? t)
         (error "cannot assign to unbound identifier:" s)
         (error "cannot assign to syntax:" s)))
   (rebuild
    s
    (list (m 'set!)
          (m 'id)
          (expand (m 'rhs) ctx)))))
