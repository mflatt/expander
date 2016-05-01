#lang racket/unit
(require racket/unit
         "stx.rkt"
         "scope.rkt"
         "pattern.rkt"
         "namespace.rkt"
         "binding.rkt"
         "dup-check.rkt"
         "expand-context.rkt"
         "expand-sig.rkt")

(import expand^)
(export)

;; ----------------------------------------

(define (make-lambda-expander s formals bodys ctx)
  (define sc (new-scope))
  (define ids (let loop ([formals formals])
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
                 [else (error "huh?" formals)])))
  (define phase (expand-context-phase ctx))
  (check-no-duplicate-ids ids phase s)
  (define keys (for/list ([id (in-list ids)])
                 (add-local-binding! id phase)))
  (define body-env (for*/fold ([env (expand-context-env ctx)]) ([key (in-list keys)])
                     (env-extend env key 'variable)))
  (define body-ctx (struct-copy expand-context ctx [env body-env]))
  (values (add-scope (datum->syntax #f formals s) sc)
          (expand-body bodys sc s body-ctx)))

(add-core-form!
 'lambda
 (lambda (s ctx)
   (define m (parse-syntax s '(lambda formals body ...+)))
   (define-values (formals body)
     (make-lambda-expander s (m 'formals) (m 'body) ctx))
   (rebuild
    s
    `(,(m 'lambda) ,formals ,body))))

(add-core-form!
 'case-lambda
 (lambda (s ctx)
   (define m (parse-syntax s '(case-lambda [formals body ...+] ...)))
   (define cm (parse-syntax s '(case-lambda clause ...)))
   (rebuild
    s
    `(,(m 'case-lambda)
      ,@(for/list ([formals (in-list (m 'formals))]
                   [bodys (in-list (m 'body))]
                   [clause (in-list (cm 'clause))])
          (define-values (exp-formals exp-body)
            (make-lambda-expander s formals bodys ctx))
          (rebuild clause `[,exp-formals ,exp-body]))))))

;; ----------------------------------------

(define (make-let-values-form syntaxes? rec?)
  (lambda (s ctx)
    (define m (if syntaxes?
                  (parse-syntax s '(letrec-syntaxes+values
                                    ([(trans-id ...) trans-rhs] ...)
                                    ([(val-id ...) val-rhs] ...)
                                    body ...+))
                  (parse-syntax s '(let-values ([(val-id ...) val-rhs] ...)
                                    body ...+))))
   (define sc (new-scope))
   (define trans-idss (for/list ([ids (in-list (m 'trans-id))])
                        (for/list ([id (in-list ids)])
                          (add-scope id sc))))
   (define val-idss (for/list ([ids (in-list (m 'val-id))])
                      (for/list ([id (in-list ids)])
                        (add-scope id sc))))
   (define phase (expand-context-phase ctx))
   (check-no-duplicate-ids (list trans-idss val-idss) phase s)
   (define trans-keyss (for/list ([ids (in-list trans-idss)])
                         (for/list ([id (in-list ids)])
                           (add-local-binding! id phase))))
   (define val-keyss (for/list ([ids (in-list val-idss)])
                       (for/list ([id (in-list ids)])
                         (add-local-binding! id phase))))
   (define trans-valss (for/list ([rhs (in-list (m 'trans-rhs))]
                                  [ids (in-list trans-idss)])
                         (eval-for-syntaxes-binding (add-scope rhs sc) ids ctx)))
   (define rec-val-env
     (for*/fold ([env (expand-context-env ctx)]) ([keys (in-list val-keyss)]
                                                  [key (in-list keys)])
       (env-extend env key 'variable)))
   (define rec-env (for/fold ([env rec-val-env]) ([keys (in-list trans-keyss)]
                                                  [vals (in-list trans-valss)])
                     (for/fold ([env env]) ([key (in-list keys)]
                                            [val (in-list vals)])
                       (env-extend env key (local-transformer val)))))
   (define rec-ctx (struct-copy expand-context ctx [env rec-env]))
   (define letrec-values-id
     (if syntaxes?
         (datum->syntax (syntax-shift-phase-level core-stx phase) 'letrec-values)
         (m 'let-values)))
   (rebuild
    s
    `(,letrec-values-id ,(for/list ([ids (in-list val-idss)]
                                    [rhs (in-list (m 'val-rhs))])
                           `[,ids ,(expand (add-scope rhs sc) rec-ctx)])
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
   (define m (parse-syntax s '(#%datum . datum)))
   (define phase (expand-context-phase ctx))
   (rebuild
    s
    (list (datum->syntax (syntax-shift-phase-level core-stx phase) 'quote)
          (m 'datum)))))

(add-core-form!
 '#%app
 (lambda (s ctx)
   (define m (parse-syntax s '(#%app rator rand ...)))
   (rebuild
    s
    (list* (m '#%app)
           (expand (m 'rator) ctx)
           (for/list ([rand (in-list (m 'rand))])
             (expand rand ctx))))))

(add-core-form!
 'quote
 (lambda (s ctx)
   (parse-syntax s '(quote datum))
   s))

(add-core-form!
 'quote-syntax
 (lambda (s ctx)
   (parse-syntax s '(quote-syntax datum))
   s))

(add-core-form!
 'if
 (lambda (s ctx)
   (define m (parse-syntax s '(if tst thn els)))
   (rebuild
    s
    (list (m 'if)
          (expand (m 'tst) ctx)
          (expand (m 'thn) ctx)
          (expand (m 'els) ctx)))))

(add-core-form!
 'with-continuation-mark
 (lambda (s ctx)
   (define m (parse-syntax s '(with-continuation-mark key val body)))
   (rebuild
    s
    (list (m 'with-continuation-mark)
          (expand (m 'key) ctx)
          (expand (m 'val) ctx)
          (expand (m 'body) ctx)))))

(define (make-begin)
 (lambda (s ctx)
   (define m (parse-syntax s '(begin e ...+)))
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
   (define m (parse-syntax s '(set! id rhs)))
   (define binding (resolve (m 'id) (expand-context-phase ctx)))
   (unless binding
     (error "no binding for assignment:" s))
   (define t (lookup binding ctx s))
   (unless (variable? t)
     (error "cannot assign to syntax:" s))
   (rebuild
    s
    (list (m 'set!)
          (m 'id)
          (expand (m 'rhs) ctx)))))
