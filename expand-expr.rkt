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
         "expand.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "debug.rkt")

;; ----------------------------------------

;; Common expansion for `lambda` and `case-lambda`
(define (lambda-clause-expander s formals bodys ctx)
  (define sc (new-scope 'local))
  (define phase (expand-context-phase ctx))
  ;; Parse and check formal arguments:
  (define ids (parse-and-flatten-formals formals sc))
  (check-no-duplicate-ids ids phase s)
  ;; Bind each argument and generate a corresponding key for the
  ;; expand-time environment:
  (define keys (for/list ([id (in-list ids)])
                 (add-local-binding! id phase)))
  (define body-env (for*/fold ([env (expand-context-env ctx)]) ([key (in-list keys)])
                     (env-extend env key variable)))
  ;; Expand the function body:
  (define body-ctx (struct-copy expand-context ctx
                                [env body-env]
                                [scopes (cons sc (expand-context-scopes ctx))]
                                [all-scopes-stx
                                 (add-scope (expand-context-all-scopes-stx ctx) sc)]))
  (define exp-body (expand-body bodys sc s body-ctx))
  ;; Return formals (with new scope) and expanded body:
  (values (add-scope formals sc)
          exp-body))

(define (expand-lambda s ctx)
  (define m (match-syntax s '(lambda formals body ...+)))
  (define-values (formals body)
    (lambda-clause-expander s (m 'formals) (m 'body) ctx))
  (rebuild
   s
   `(,(m 'lambda) ,formals ,body)))

(add-core-form!
 'lambda
 expand-lambda)

(add-core-form!
 'λ
 expand-lambda)

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
            (lambda-clause-expander s formals bodys ctx))
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
   (define sc (new-scope 'local))
   (define phase (expand-context-phase ctx))
   ;; Add the new scope to each binding identifier:
   (define trans-idss (for/list ([ids (in-list (if syntaxes? (m 'trans-id) null))])
                        (for/list ([id (in-list ids)])
                          (add-scope id sc))))
   (define val-idss (for/list ([ids (in-list (m 'val-id))])
                      (for/list ([id (in-list ids)])
                        (add-scope id sc))))
   (check-no-duplicate-ids (list trans-idss val-idss) phase s)
   ;; Bind each left-hand identifier and generate a corresponding key
   ;; fo the expand-time environment:
   (define trans-keyss (for/list ([ids (in-list trans-idss)])
                         (for/list ([id (in-list ids)])
                           (add-local-binding! id phase))))
   (define val-keyss (for/list ([ids (in-list val-idss)])
                       (for/list ([id (in-list ids)])
                         (add-local-binding! id phase))))
   ;; Evaluate compile-time expressions (if any):
   (define trans-valss (for/list ([rhs (in-list (if syntaxes? (m 'trans-rhs) '()))]
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
   ;; Expand right-hand sides and body
   (define expr-ctx (as-expression-context ctx))
   (define rec-ctx (struct-copy expand-context expr-ctx
                                [env rec-env]
                                [scopes (cons sc (expand-context-scopes ctx))]
                                [all-scopes-stx
                                 (add-scope (expand-context-all-scopes-stx ctx) sc)]))
   (define letrec-values-id
     (if syntaxes?
         (datum->syntax (syntax-shift-phase-level core-stx phase) 'letrec-values)
         (m 'let-values)))
   (rebuild
    s
    `(,letrec-values-id ,(for/list ([ids (in-list val-idss)]
                                    [rhs (in-list (m 'val-rhs))])
                           `[,ids ,(if rec?
                                       (expand (add-scope rhs sc)
                                               (as-named-context rec-ctx ids))
                                       (expand rhs
                                               (as-named-context expr-ctx ids)))])
      ,(expand-body (m 'body) sc s (as-tail-context rec-ctx #:wrt ctx))))))

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
   (define phase (expand-context-phase ctx))
   (rebuild
    s
    (list (datum->syntax (syntax-shift-phase-level core-stx phase) 'quote)
          (m 'datum)))))

;; Sensible `#%app` disallows empty combinations
#|
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
|#

;; '#%kernel `#%app` treats an empty combination as a literal null
(add-core-form!
 '#%app
 (lambda (s ctx)
   (define m (match-syntax s '(#%app e ...)))
   (define es (m 'e))
   (cond
    [(null? es)
     (define phase (expand-context-phase ctx))
     (rebuild
      s
      (list (datum->syntax (syntax-shift-phase-level core-stx phase) 'quote)
            null))]
    [else
     (define expr-ctx (as-expression-context ctx))
     (rebuild
      s
      (list* (m '#%app)
             (for/list ([e (in-list es)])
               (expand e expr-ctx))))])))

(add-core-form!
 'quote
 (lambda (s ctx)
   (match-syntax s '(quote datum))
   s))

(add-core-form!
 'quote-syntax
 (lambda (s ctx)
   (define m-local (try-match-syntax s '(quote-syntax datum #:local)))
   (define m (or m-local
                 (match-syntax s '(quote-syntax datum))))
   (rebuild
    s
    `(,(m 'quote-syntax)
      ,(if m-local
           ;; #:local means don't prune:
           (m 'datum)
           ;; otherwise, prune scopes up to transformer boundary:
           (remove-scopes (m 'datum) (expand-context-scopes ctx)))))))

(add-core-form!
 'if
 (lambda (s ctx)
   (define m (match-syntax s '(if tst thn els)))
   (define expr-ctx (as-expression-context ctx))
   (define tail-ctx (as-tail-context expr-ctx #:wrt ctx))
   (rebuild
    s
    (list (m 'if)
          (expand (m 'tst) expr-ctx)
          (expand (m 'thn) tail-ctx)
          (expand (m 'els) tail-ctx)))))

(add-core-form!
 'with-continuation-mark
 (lambda (s ctx)
   (define m (match-syntax s '(with-continuation-mark key val body)))
   (define expr-ctx (as-expression-context ctx))
   (rebuild
    s
    (list (m 'with-continuation-mark)
          (expand (m 'key) expr-ctx)
          (expand (m 'val) expr-ctx)
          (expand (m 'body) (as-tail-context expr-ctx #:wrt ctx))))))

(define (make-begin)
 (lambda (s ctx)
   (define m (match-syntax s '(begin e ...+)))
   (define expr-ctx (as-expression-context ctx))
   (define es (m 'e))
   (define last-i (sub1 (length es)))
   (rebuild
    s
    (cons (m 'begin)
          (for/list ([e (in-list es)]
                     [i (in-naturals)])
            (expand e (if (= i last-i)
                          (as-tail-context expr-ctx #:wrt ctx)
                          expr-ctx)))))))

(add-core-form!
 'begin
 (make-begin))

(add-core-form!
 'begin0
 (make-begin))

(define (register-eventual-variable!? id ctx)
  (cond
   [(expand-context-need-eventually-defined ctx)
    ;; In top level or `begin-for-syntax`, encountered a reference to a
    ;; variable that might be defined later; record it for later checking
    (hash-update! (expand-context-need-eventually-defined ctx)
                  (expand-context-phase ctx)
                  (lambda (l) (cons id l))
                  null)
    #t]
   [else #f]))

(add-core-form!
 '#%top
 (lambda (s ctx)
   (define m (match-syntax s '(#%top . id)))
   (define id (m 'id))
   (cond
    [(register-eventual-variable!? id ctx)
     id]
    [else
     (error "unbound identifier:" (m 'id)
            (syntax-debug-info (m 'id)
                               (expand-context-phase ctx)
                               #t))])))

(add-core-form!
 'set!
 (lambda (s ctx)
   (define m (match-syntax s '(set! id rhs)))
   (define id (m 'id))
   (define binding (resolve+shift id (expand-context-phase ctx)))
   (define t (and binding (lookup binding ctx s)))
   (cond
    [(or (variable? t)
         (and (not binding)
              (register-eventual-variable!? id ctx)))
     (rebuild
      s
      (list (m 'set!)
            id
            (expand (m 'rhs) (as-expression-context ctx))))]
    [(not binding)
     (error "no binding for assignment:" s)]
    [(set!-transformer? t)
     (expand (apply-transformer (transformer->procedure t) s ctx binding) ctx)]
    [(rename-transformer? t)
     (expand (datum->syntax s
                            (list (m 'set!)
                                  (rename-transformer-target t)
                                  (m 'rhs))
                            s
                            s)
             ctx)]
    [else (error "cannot assign to syntax:" s)])))

(add-core-form!
 '#%variable-reference
 (lambda (s ctx)
   (define id-m (try-match-syntax s '(#%variable-reference id)))
   (define top-m (and (not id-m)
                      (try-match-syntax s '(#%variable-reference (#%top . id)))))
   (define empty-m (and (not id-m)
                        (not top-m)
                        (match-syntax s '(#%variable-reference))))
   (when (or id-m top-m)
     (define binding (resolve+shift ((or id-m top-m) 'id) (expand-context-phase ctx)))
     (unless binding
       (error "no binding for variable reference:" s)))
   
   s))

(add-core-form!
 '#%expression
 (lambda (s ctx)
   (define m (match-syntax s '(#%expression e)))
   (define exp-e (expand (m 'e) (as-tail-context (as-expression-context ctx)
                                                 #:wrt ctx)))
   (case (expand-context-context ctx)
     [(expression) exp-e]
     [else (rebuild
            s
            `(,(m '#%expression) ,exp-e))])))
