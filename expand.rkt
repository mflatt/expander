#lang racket/base
(require racket/unit
         "stx.rkt"
         "scope.rkt"
         "pattern.rkt"
         "namespace.rkt"
         "binding.rkt"
         "dup-check.rkt"
         "compile.rkt"
         "require.rkt"
         "expand-context.rkt"
         "expand-sig.rkt"
         "expand-expr.rkt"
         "expand-module.rkt"
         "expand-top-level.rkt")

(provide expand)

;; exports are via "expand-sig.rkt"

;; ----------------------------------------

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
                  (expand-context-phase ctx)
                  id))

;; ----------------------------------------

(define (expand-transformer s ctx)
  (expand s (struct-copy expand-context ctx
                         [phase (add1 (expand-context-phase ctx))]
                         [env empty-env]
                         [only-immediate? #f]
                         [add-scope #f]
                         [current-module-scopes null])))

(define (eval-transformer s ctx)
  (eval `(#%expression ,(compile s
                                 (add1 (expand-context-phase ctx))
                                 (expand-context-namespace ctx)))
        (make-base-namespace)))

;; ----------------------------------------

(define core-scope (new-multi-scope))
(define core-stx (add-scope empty-stx core-scope))

(define core-transformers #hasheq())
(define core-primitives #hasheq())

(define (add-core-form! sym proc)
  (add-binding! (datum->syntax core-stx sym)
                (module-binding '#%core 0 sym
                                '#%core 0 sym
                                0)
                0)
  (set! core-transformers (hash-set core-transformers
                                    sym
                                    proc)))

(define (add-core-primitive! sym val)
  (add-binding! (datum->syntax core-stx sym)
                (module-binding '#%core 0 sym
                                '#%core 0 sym
                                0)
                0)
  (set! core-primitives (hash-set core-primitives
                                  sym
                                  val)))

;; ----------------------------------------

(define (add-local-binding! id phase)
  (define key (gensym))
  (add-binding! id (local-binding key) phase)
  key)


(define (rebuild orig-s new)
  (datum->syntax orig-s new orig-s orig-s))
  
;; ----------------------------------------

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
         (define vals (eval-for-syntaxes-binding (m 'rhs) ids ctx))
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

(define (no-binds s phase)
  (define s-core-stx (syntax-shift-phase-level core-stx phase))
  (list null null (datum->syntax #f
                                 `(,(datum->syntax s-core-stx 'begin)
                                   (,(datum->syntax s-core-stx '#%app)
                                    ,(datum->syntax s-core-stx 'values)))
                                 s)))

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

(define (expand+eval-for-syntaxes-binding rhs ids ctx)
  (define exp-rhs (expand-transformer rhs ctx))
  (define vals
    (call-with-values (lambda () (eval-transformer exp-rhs ctx))
      list))
  (unless (= (length vals) (length ids))
    (error "wrong number of results (" (length vals) "vs." (length ids) ")"
           "from" rhs))
  (values exp-rhs vals))

(define (eval-for-syntaxes-binding rhs ids ctx)
  (define-values (exp-rhs vals)
    (expand+eval-for-syntaxes-binding rhs ids ctx))
  vals)

;; ----------------------------------------

(invoke-unit expand-expr@ (import expand^))
(invoke-unit expand-module@ (import expand^))
(invoke-unit expand-top-level@ (import expand^))

;; ----------------------------------------

(add-core-primitive! 'syntax-e syntax-e)
(add-core-primitive! 'car car)
(add-core-primitive! 'cdr cdr)
(add-core-primitive! 'values values)

;; ----------------------------------------

(define core-module
  (make-module null
               (hasheqv 0 (for/hasheq ([sym (in-hash-keys core-primitives)])
                            (values sym (module-binding '#%core 0 sym
                                                        '#%core 0 sym
                                                        0))))
               (hasheqv 0 (for/hasheq ([sym (in-hash-keys core-transformers)])
                            (values sym (module-binding '#%core 0 sym
                                                        '#%core 0 sym
                                                        0))))
               0 1
               (lambda (ns phase phase-level)
                 (case phase-level
                   [(0)
                    (for ([(sym val) (in-hash core-primitives)])
                      (namespace-set-variable! ns 0 sym val))]
                   [(1)
                    (for ([(sym proc) (in-hash core-transformers)])
                      (namespace-set-transformer! ns 0 sym (core-form proc)))]))))

(declare-module! (current-namespace) '#%core core-module)

;; ----------------------------------------

(define demo-scope (new-multi-scope))
(define demo-stx (add-scope empty-stx demo-scope))

(stx-context-require! demo-stx 0 (current-namespace) '#%core)
(stx-context-require! demo-stx 1 (current-namespace) '#%core)

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

;; ----------------------------------------

(expand (datum->syntax demo-stx '(module m '#%core
                                  (#%require (for-syntax '#%core))
                                  (define-syntaxes (m) (lambda (stx) (quote-syntax 10)))
                                  (define-values (x) 1)
                                  (#%provide (prefix-all-defined-except def: x))
                                  (m)))
        (struct-copy expand-context (current-expand-context)
                     [context 'top-level]
                     [current-module-scopes (list demo-scope)]))
