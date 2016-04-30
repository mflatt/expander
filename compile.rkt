#lang racket/base
(require "stx.rkt"
         "scope.rkt"
         "namespace.rkt"
         "binding.rkt"
         "pattern.rkt")

(provide compile)

(define (compile s [phase 0] [ns (current-namespace)] [env #hasheq()])
  (cond
   [(pair? (syntax-e s))
    (define core-sym (core-form-sym s phase))
    (case core-sym
      [(#f)
       (error "not a core form:" s)]
      [(lambda)
       (define m (parse-syntax s '(lambda formals body)))
       `(lambda ,@(compile-lambda (m 'formals) (m 'body) phase ns env))]
      [(case-lambda)
       (define m (parse-syntax s '(case-lambda [formals body] ...)))
       `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                             [body (in-list (m 'body))])
                    (compile-lambda formals body phase ns env)))]
      [(#%app)
       (define m (parse-syntax s '(#%app . rest)))
       (for/list ([s (in-list (m 'rest))])
         (compile s phase ns env))]
      [(if)
       (define m (parse-syntax s '(if tst thn els)))
       `(if
         ,(compile (m 'tst) phase ns env)
         ,(compile (m 'thn) phase ns env)
         ,(compile (m 'els) phase ns env))]
      [(with-continuation-mark)
       (define m (parse-syntax s '(if key val body)))
       `(with-continuation-mark
         ,(compile (m 'key) phase ns env)
         ,(compile (m 'val) phase ns env)
         ,(compile (m 'body) phase ns env))]
      [(begin begin0)
       (define m (parse-syntax s '(begin e ...+)))
       `(,core-sym ,@(for/list ([e (in-list (m 'e))])
                       (compile e phase ns env)))]
      [(set!)
       (define m (parse-syntax s '(set! id rhs)))
       `(set!
         ,(compile (m 'id) phase ns env)
         ,(compile (m 'rhs) phase ns env))]
      [(let-values letrec-values)
       (define rec? (eq? core-sym 'letrec-values))
       (define m (parse-syntax s '(let-values ([(id ...) rhs] ...) body)))
       (define sc (new-scope))
       (define idss (m 'id))
       (define keyss (for/list ([ids (in-list idss)])
                       (for/list ([id (in-list ids)])
                         (define b (resolve id phase))
                         (unless (local-binding? b)
                           (error "bad binding:" id))
                         (local-binding-key b))))
       (define rec-env (for/fold ([env env]) ([ids (in-list idss)]
                                              [keys (in-list keyss)])
                         (for/fold ([env env]) ([id (in-list ids)]
                                                [key (in-list keys)])
                           (hash-set env key (gensym (syntax-e id))))))
       `(,core-sym ,(for/list ([keys (in-list keyss)]
                               [rhs (in-list (m 'rhs))])
                      `[,(for/list ([key (in-list keys)])
                           (hash-ref rec-env key))
                        ,(compile rhs phase ns rec-env)])
         ,(compile (m 'body) phase ns rec-env))]
      [(quote)
       (define m (parse-syntax s '(quote datum)))
       `(quote ,(syntax->datum (m 'datum)))]
      [(quote-syntax)
       (define m (parse-syntax s '(quote datum)))
       `(quote ,(m 'datum))]
      [else
       (error "unrecognized core form:" core-sym)])]
   [(identifier? s)
    (define b (resolve s phase))
    (cond
     [(local-binding? b)
      (define sym (hash-ref env (local-binding-key b) #f))
      (unless sym
        (error "missing a binding after expansion:" s))
      sym]
     [(module-binding? b)
      (define m (namespace->module-namespace ns (module-binding-module b)))
      (namespace-get-variable ns (module-binding-phase b) (module-binding-def b) 'undefined)]
     [else
      (error "not a reference to a local binding:" s)])]
   [else
    (error "bad syntax after expansion:" s)]))

(define (compile-lambda formals body phase ns env)
  (define gen-formals
    (let loop ([formals formals])
      (cond
       [(identifier? formals) (gensym (syntax-e formals))]
       [(syntax? formals) (loop (syntax-e formals))]
       [(pair? formals) (cons (loop (car formals))
                              (loop (cdr formals)))]
       [else null])))
  (define body-env
    (let loop ([formals formals]
               [gen-formals gen-formals]
               [env env])
      (cond
       [(identifier? formals)
        (define b (resolve formals phase))
        (unless (local-binding? b)
          (error "bad binding:" formals))
        (hash-set env (local-binding-key b) gen-formals)]
       [(syntax? formals) (loop (syntax-e formals) gen-formals env)]
       [(pair? formals)
        (loop (cdr formals) (cdr gen-formals)
              (loop (car formals) (car gen-formals) env))]
       [else env])))
  `(,gen-formals ,(compile body phase ns body-env)))
