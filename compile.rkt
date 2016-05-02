#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "namespace.rkt"
         "binding.rkt"
         "pattern.rkt")

(provide compile
         expand-time-eval
         run-time-eval)

(define empty-env #hasheq())

(define phase-id (gensym 'phase))

(define (compile s [phase 0] [ns (current-namespace)] [env empty-env])
  (cond
   [(pair? (syntax-e s))
    (define core-sym (core-form-sym s phase))
    (case core-sym
      [(#f)
       (error "not a core form:" s)]
      [(module)
       (define m (parse-syntax s '(module name import form ...)))
       (compile-module (syntax-e (m 'name))
                       (syntax-property s 'module-requires)
                       (syntax-property s 'module-provides)
                       (m 'form)
                       ns)]
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
      (define mod-name (module-binding-module b))
      (cond
       [(equal? mod-name '#%core)
        (define m-ns (namespace->module-namespace ns mod-name phase))
        (or (namespace-get-variable m-ns (module-binding-phase b) (module-binding-sym b) #f)
            (error "internal error: bad #%core reference"))]
       [(equal? mod-name 'self)
        (module-binding-sym b)]
       [else
        `(namespace-get-variable
          (namespace->module-namespace (current-namespace)
                                       ',mod-name
                                       (+ ,phase-id
                                          ,(module-binding-nominal-import-phase b)))
          ,(module-binding-phase b)
          ',(module-binding-sym b)
          'undefined)])]
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



;; ----------------------------------------
                       
(define (compile-module name requires provides bodys ns)
  (define ns-id (gensym 'namespace))
  (define phase-level-id (gensym 'phase-level))
  
  (define (add-body phase-to-body phase body)
    (hash-update phase-to-body phase (lambda (l) (cons body l)) null))
  
  (define phase-to-bodys
    (let loop ([bodys bodys]
               [phase 0]
               [phase-to-body #hasheqv()])
      (cond
       [(null? bodys) phase-to-body]
       [else
        (case (core-form-sym (car bodys) phase)
          [(define-values)
           (define m (parse-syntax (car bodys) '(define-values (id ...) rhs)))
           (define syms (def-ids-to-syms (m 'id) phase))
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  phase
                  `(begin
                    (define-values ,syms ,(compile (m 'rhs) phase ns empty-env))
                    ,@(for/list ([sym (in-list syms)])
                        `(namespace-set-variable! ,ns-id ,phase-id ',sym ,sym)))))]
          [(define-syntaxes)
           (define m (parse-syntax (car bodys) '(define-syntaxes (id ...) rhs)))
           (define syms (def-ids-to-syms (m 'id) phase))
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  (add1 phase)
                  `(let-values ([,syms ,(compile (m 'rhs) (add1 phase) ns empty-env)])
                    ,@(for/list ([sym (in-list syms)])
                        `(namespace-set-transformer! ,ns-id (sub1 ,phase-id) ',sym ,sym)))))]
          [(#%require #%provide)
           (loop (cdr bodys)
                 phase
                 phase-to-body)]
          [else
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  phase
                  (compile (car bodys) phase ns empty-env)))])])))
  
  (define-values (min-phase max-phase)
    (for/fold ([min-phase 0] [max-phase 0]) ([phase (in-hash-keys phase-to-bodys)])
      (values (min min-phase phase)
              (max max-phase phase))))

  `(declare-module!
    (current-namespace)
    ',name
    (make-module
     ,requires
     ,provides
     ,min-phase
     ,max-phase
     (lambda (,ns-id ,phase-id ,phase-level-id)
       (case ,phase-level-id
         ,@(for/list ([(phase bodys) (in-hash phase-to-bodys)])
             `[(,phase)
               (let ([,phase-id (+ ,phase-id ,phase-level-id)])
                 ,@(reverse bodys))]))
       (void)))))
         
(define (def-ids-to-syms ids phase)
  (for/list ([id (in-list ids)])
    (define b (resolve id phase))
    (unless (and (module-binding? b)
                 (eq? 'self (module-binding-module b))
                 (eqv? phase (module-binding-phase b)))
      (error "bad binding for module definition:" id))
    (module-binding-sym b)))


(define expand-time-namespace (make-base-namespace))
(define (expand-time-eval compiled)
  (eval compiled expand-time-namespace))

(define run-time-namespace (make-base-namespace))
(define (add-run-time! sym val)
  (namespace-set-variable-value! sym val #t run-time-namespace))

(add-run-time! 'make-module make-module)
(add-run-time! 'declare-module! declare-module!)
(add-run-time! 'current-namespace current-namespace)
(add-run-time! 'namespace-set-variable! namespace-set-variable!)
(add-run-time! 'namespace-set-transformer! namespace-set-transformer!)
(add-run-time! 'namespace-get-variable namespace-get-variable)
(add-run-time! 'namespace->module-namespace namespace->module-namespace)

(define (run-time-eval compiled)
  (eval compiled run-time-namespace))
