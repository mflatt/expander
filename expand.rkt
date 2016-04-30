#lang racket/base
(require "stx.rkt"
         "scope.rkt"
         "pattern.rkt"
         "namespace.rkt"
         "binding.rkt"
         "dup-check.rkt"
         "compile.rkt")

(struct expand-context (phase      ; current expansion phase
                        namespace  ; namespace for modules and top-levels
                        env        ; environment for local bindings
                        only-immediate? ; #t => stop at core forms
                        add-scope  ; scope to add to every expansion; #f if none
                        ))

(define current-expand-context (make-parameter
                                (expand-context 0
                                                (current-namespace)
                                                empty-env
                                                #f
                                                #f)))

(define (expand s [ctx (current-expand-context)])
  (cond
   [(identifier? s)
    (define binding (resolve s (expand-context-phase ctx)))
    (cond
     [(not binding)
      (expand-implicit '#%top s ctx)]
     [else
      (dispatch (lookup binding ctx s) s ctx)])]
   [(and (pair? (syntax-e s))
         (identifier? (car (syntax-e s))))
    (define id (car (syntax-e s)))
    (define binding (resolve id (expand-context-phase ctx)))
    (cond
     [(not binding) (expand-implicit '#%app s ctx)]
     [else
      (define t (lookup binding ctx id))
      (cond
       [(variable? t) (expand-implicit '#%app s ctx)]
       [else (dispatch t s ctx)])])]
   [(pair? (syntax-e s))
    (expand-implicit '#%app s ctx)]
   [(null? (syntax-e s))
    (error "empty application:" s)]
   [else
    (expand-implicit '#%datum s ctx)]))

(define (expand-implicit sym s ctx)
  (define id (datum->syntax s sym))
  (define b (resolve id (expand-context-phase ctx)))
  (define t (and b (lookup b ctx id)))
  (cond
   [(core-form? t)
    (if (expand-context-only-immediate? ctx)
        s
        (dispatch t (datum->syntax s (cons sym s) s) ctx))]
   [(transformer? t)
    (dispatch t (datum->syntax s (cons sym s) s) ctx)]
   [else
    (error (format "no transformer binding for ~a:" sym)
           s)]))

(define (dispatch t s ctx)
  (cond
   [(core-form? t)
    (if (expand-context-only-immediate? ctx)
        s
        ((core-form-expander t) s ctx))]
   [(transformer? t)
    (define exp-s (parameterize ([current-expand-context ctx])
                    (t s)))
    (define next-s (if (expand-context-add-scope ctx)
                       (add-scope exp-s (expand-context-add-scope ctx))
                       exp-s))
    (expand next-s ctx)]
   [(variable? t) s]
   [else (error "unknown transformer for dispatch:" t)]))

(define (lookup b ctx id)
  (binding-lookup b
                  (expand-context-env ctx)
                  (expand-context-namespace ctx)
                  id))

;; ----------------------------------------

(define (expand-transformer s ctx)
  (expand s (struct-copy expand-context ctx
                         [phase (add1 (expand-context-phase ctx))]
                         [env empty-env])))

(define (eval-transformer s ctx)
  (eval `(#%expression ,(compile s
                                 (add1 (expand-context-phase ctx))
                                 core-module))
        (make-base-namespace)))

;; ----------------------------------------

(define core-module
  (namespace->module-namespace (current-namespace)
                               '#%core))

(define core-scope (new-multi-scope))
(define core-stx (add-scope empty-stx core-scope))

(define (add-core-form! sym proc)
  (add-binding! (datum->syntax core-stx sym)
                (module-binding '#%core 0 sym)
                0)
  (namespace-set-transformer! core-module
                              0
                              sym
                              (core-form proc)))

(define (add-local-binding! id phase)
  (define key (gensym))
  (add-binding! id (local-binding key) phase)
  key)


(define (rebuild orig-s new)
  (datum->syntax orig-s new orig-s orig-s))
  
;; ----------------------------------------

(define (no-binds s phase)
  (define s-core-stx (syntax-shift-phase-level core-stx phase))
  (list null null (datum->syntax #f
                                 `(,(datum->syntax s-core-stx 'begin)
                                   (,(datum->syntax s-core-stx '#%app)
                                    ,(datum->syntax s-core-stx 'values)))
                                 s)))

(define (expand-body bodys sc s ctx)
  (define outside-sc (new-scope))
  (define inside-sc (new-scope))
  (define phase (expand-context-phase ctx))
  (define body-ctx (struct-copy expand-context ctx
                                [only-immediate? #t]
                                [add-scope inside-sc]))
  (let loop ([body-ctx body-ctx]
             [bodys (for/list ([body (in-list bodys)])
                      (add-scope (add-scope (add-scope body sc) outside-sc) inside-sc))]
             [done-bodys null]
             [trans-binds null] ; for `expand-once`, eventually
             [val-binds null]
             [dups (make-check-no-duplicate-table)])
    (cond
     [(null? bodys)
      (finish-expanding-body body-ctx done-bodys val-binds s)]
     [else
      (define exp-body (expand (car bodys) body-ctx))
      (case (core-form-sym exp-body phase)
        [(begin)
         (define m (parse-syntax exp-body '(begin e ...)))
         (loop body-ctx
               (append (m 'e) (cdr bodys))
               done-bodys
               trans-binds
               val-binds
               dups)]
        [(define-values)
         (define m (parse-syntax exp-body '(define-values (id ...) rhs)))
         (define ids (m 'id))
         (define new-dups (check-no-duplicate-ids ids phase exp-body dups))
         (define keys (for/list ([id (in-list ids)])
                        (add-local-binding! id phase)))
         (define extended-env (for/fold ([env (expand-context-env body-ctx)]) ([key (in-list keys)])
                                (env-extend env key 'variable)))
         (loop (struct-copy expand-context body-ctx
                            [env extended-env])
               (cdr bodys)
               null
               trans-binds
               (cons (list ids (m 'rhs))
                     (append
                      (for/list ([done-body (in-list done-bodys)])
                        (no-binds s phase))
                      val-binds))
               new-dups)]
        [(define-syntaxes)
         (define m (parse-syntax exp-body '(define-syntaxes (id ...) rhs)))
         (define ids (m 'id))
         (define new-dups (check-no-duplicate-ids ids phase exp-body dups))
         (define keys (for/list ([id (in-list ids)])
                        (add-local-binding! id phase)))
         (define vals (eval-for-letrec-syntaxes (m 'rhs) ids ctx))
         (define extended-env (for/fold ([env (expand-context-env body-ctx)]) ([key (in-list keys)]
                                                                               [val (in-list vals)])
                                (env-extend env key (local-transformer val))))
         (loop (struct-copy expand-context body-ctx
                            [env extended-env])
               (cdr bodys)
               done-bodys
               (cons (list ids (m 'rhs))
                     trans-binds)
               val-binds
               new-dups)]
        [else
         (loop body-ctx
               (cdr bodys)
               (cons (car bodys) done-bodys)
               trans-binds
               val-binds
               dups)])])))

(define (finish-expanding-body body-ctx done-bodys val-binds s)
  (when (null? done-bodys)
    (error "no body forms:" s))
  (define s-core-stx
    (syntax-shift-phase-level core-stx (expand-context-phase body-ctx)))
  (define finish-ctx (struct-copy expand-context body-ctx
                                  [only-immediate? #f]
                                  [add-scope #f]))
  (define (finish-bodys)
    (cond
     [(null? (cdr done-bodys))
      (expand (car done-bodys) finish-ctx)]
     [else
      (datum->syntax
       #f
       `(,(datum->syntax s-core-stx 'begin)
         ,@(for/list ([body (in-list done-bodys)])
             (expand body finish-ctx)))
       s)]))
  (cond
   [(null? val-binds)
    (finish-bodys)]
   [else
    (datum->syntax
     #f
     `(,(datum->syntax s-core-stx 'letrec-values)
       ,(for/list ([bind (in-list (reverse val-binds))])
          `(,(datum->syntax #f (car bind)) ,(expand (cadr bind) finish-ctx)))
       ,(finish-bodys))
     s)]))



(define (eval-for-letrec-syntaxes rhs ids ctx)
  (define vals
    (call-with-values (lambda ()
                        (eval-transformer
                         (expand-transformer rhs ctx)
                         ctx))
      list))
  (unless (= (length vals) (length ids))
    (error "wrong number of results (" (length vals) "vs." (length ids) ")"
           "from" rhs))
  vals)

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
   
(define (make-let-values-form rec?)
 (lambda (s ctx)
   (define m (parse-syntax s '(let-values ([(id ...) rhs] ...) body ...+)))
   (define sc (new-scope))
   (define idss (for/list ([ids (in-list (m 'id))])
                  (for/list ([id (in-list ids)])
                    (add-scope id sc))))
   (define phase (expand-context-phase ctx))
   (check-no-duplicate-ids idss phase s)
   (define keyss (for/list ([ids (in-list idss)])
                   (for/list ([id (in-list ids)])
                     (add-local-binding! id phase))))
   (define body-env (for*/fold ([env (expand-context-env ctx)]) ([keys (in-list keyss)]
                                                                 [key (in-list keys)])
                      (env-extend env key 'variable)))
   (define body-ctx (struct-copy expand-context ctx [env body-env]))
   (rebuild
    s
    `(,(m 'let-values) ,(for/list ([ids (in-list idss)]
                                   [rhs (in-list (m 'rhs))])
                          `[,ids ,(expand (if rec? (add-scope rhs sc) rhs)
                                          (if rec? body-ctx ctx))])
      ,(expand-body (m 'body) sc s body-ctx)))))

(add-core-form!
 'let-values
 (make-let-values-form #f))

(add-core-form!
 'letrec-values
 (make-let-values-form #t))

(add-core-form!
 'letrec-syntaxes+values
 (lambda (s ctx)
   (define m (parse-syntax s '(letrec-syntaxes+values
                               ([(trans-id ...) trans-rhs] ...)
                               ([(val-id ...) val-rhs] ...)
                               body ...+)))
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
                         (eval-for-letrec-syntaxes (add-scope rhs sc) ids ctx)))
   (define rec-val-env (for*/fold ([env (expand-context-env ctx)]) ([keys (in-list val-keyss)]
                                                                    [key (in-list keys)])
                         (env-extend env key 'variable)))
   (define rec-env (for/fold ([env rec-val-env]) ([keys (in-list trans-keyss)]
                                                  [vals (in-list trans-valss)])
                     (for/fold ([env env]) ([key (in-list keys)]
                                            [val (in-list vals)])
                       (env-extend env key (local-transformer val)))))
   (define rec-ctx (struct-copy expand-context ctx [env rec-env]))
   (define letrec-values-id (datum->syntax (syntax-shift-phase-level core-stx phase) 'letrec-values))
   (rebuild
    s
    `(,letrec-values-id ,(for/list ([ids (in-list val-idss)]
                                    [rhs (in-list (m 'val-rhs))])
                           `[,ids ,(expand (add-scope rhs sc) rec-ctx)])
      ,(expand-body (m 'body) sc s rec-ctx)))))

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

(add-core-form!
 'define-values
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))

(add-core-form!
 'define-syntaxes
 (lambda (s ctx)
   (error "not allowed in an expression position:" s)))

;; ----------------------------------------

(define (add-core-primitive! sym val)
  (add-binding! (datum->syntax core-stx sym)
                (module-binding '#%core 0 sym)
                0)
  (namespace-set-variable! core-module
                           0
                           sym
                           val))


(add-core-primitive! 'syntax-e syntax-e)
(add-core-primitive! 'car car)
(add-core-primitive! 'cdr cdr)
(add-core-primitive! 'values values)

;; ----------------------------------------

(define demo-stx (add-scope core-stx (shift-multi-scope core-scope 1)))

(expand (datum->syntax demo-stx '(lambda (x) x)))
(compile (expand (datum->syntax demo-stx '(case-lambda
                                           [(x) (set! x 5)]
                                           [(x y) (begin0 y x)]
                                           [() (with-continuation-mark 1 2 3)]))))
(compile (expand (datum->syntax demo-stx '(lambda (x) (define-values (y) x) y))))
(compile (expand (datum->syntax demo-stx '(lambda (x)
                                           (define-syntaxes (y) (lambda (stx) (quote-syntax 7)))
                                           y))))
                                                    
(compile (expand (datum->syntax demo-stx '(let-values ([(z) 9])
                                           (letrec-syntaxes+values
                                            ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])
                                            ([(x) 5] [(y) (lambda (z) z)])
                                            (let-values ([(z) 10])
                                              (begin z (if (m 10) 1 2))))))))
