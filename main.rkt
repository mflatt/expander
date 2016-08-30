#lang racket
(require "match.rkt")

(provide expand
         compile
         (rename-out [eval-compiled eval])

         datum->syntax
         namespace-syntax-introduce)

;; We include tests to serve as examples of various functions work
(module+ test
  (require rackunit)
  (define (make-exn:fail? rx)
    (lambda (v)
      (and (exn:fail? v) (regexp-match? rx (exn-message v))))))

;; ----------------------------------------
;; Syntax objects

(struct syntax (e        ; a non-pair atom or a list of syntax
                scopes)  ; a set of scopes
        #:transparent)

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-e s))))

(module+ test
  (check-equal? (identifier? (syntax 'x (seteq)))
                #t)
  (check-equal? (identifier? (syntax 1 (seteq)))
                #f))

;; If identifiers are `bound-identier=?`, they are fully
;; interchangable: same symbol and same scopes
(define (bound-identifier=? a b)
  (and (eq? (syntax-e a) (syntax-e b))
       (equal? (syntax-scopes a) (syntax-scopes b))))

(module+ test
  (check-equal? (bound-identifier=? (syntax 'a (seteq))
                                    (syntax 'a (seteq)))
                #t)
  (check-equal? (bound-identifier=? (syntax 'a (seteq))
                                    (syntax 'b (seteq)))
                #f)
  (check-equal? (bound-identifier=? (syntax 'a (seteq))
                                    (syntax 'a (seteq (scope))))
                #f))

;; The `datum->syntax` function coerces to syntax with an empty scope
;; set, leaving existing syntax as-is
(define (datum->syntax v)
  (cond
   [(syntax? v) v]
   [(list? v) (syntax (map datum->syntax v)
                      (seteq))]
   [else (syntax v (seteq))]))

(module+ test
  (check-equal? (datum->syntax 1)
                (syntax 1 (seteq)))
  (check-equal? (datum->syntax 'a)
                (syntax 'a (seteq)))
  (check-equal? (datum->syntax '(a b c))
                (syntax (list (syntax 'a (seteq))
                              (syntax 'b (seteq))
                              (syntax 'c (seteq)))
                        (seteq)))
  (check-equal? (datum->syntax (list 'a
                                     (syntax 'b (seteq))
                                     'c))
                (syntax (list (syntax 'a (seteq))
                              (syntax 'b (seteq))
                              (syntax 'c (seteq)))
                        (seteq))))

;; The `syntax->datum` function discards scopes --- immediate and
;; nested --- to produce a plain S-expression:
(define (syntax->datum s)
  (let ([e (syntax-e s)])
    (cond
     [(list? e) (map syntax->datum e)]
     [else e])))

(module+ test
  (check-equal? (syntax->datum (datum->syntax 1))
                1)
  (check-equal? (syntax->datum (datum->syntax 'a))
                'a)
  (check-equal? (syntax->datum (datum->syntax '(a b c)))
                '(a b c)))

;; ----------------------------------------
;; Scopes

;; A scope is an empty record, and its identity is based on `eq?`
(struct scope ())

;; Add or flip a scope everywhere (i.e., including nested syntax)
(define (apply-scope s/e sc op)
  (cond
   [(syntax? s/e) (syntax (apply-scope (syntax-e s/e) sc op)
                          (op (syntax-scopes s/e) sc))]
   [(list? s/e) (map (lambda (s) (apply-scope s sc op)) s/e)]
   [else s/e]))

(define (add-scope s sc)
  (apply-scope s sc set-add))

(define (flip-scope s sc)
  (apply-scope s sc set-flip))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(module+ test
  (define sc1 (scope))
  (define sc2 (scope))
  
  (check-equal? (add-scope (syntax 'x (seteq)) sc1)
                (syntax 'x (seteq sc1)))
  (check-equal? (add-scope (datum->syntax '(x (y))) sc1)
                (syntax (list (syntax 'x (seteq sc1))
                              (syntax (list (syntax 'y (seteq sc1)))
                                      (seteq sc1)))
                        (seteq sc1)))
  
  (check-equal? (add-scope (add-scope (syntax 'x (seteq)) sc1) sc2)
                (syntax 'x (seteq sc1 sc2)))
  (check-equal? (add-scope (add-scope (syntax 'x (seteq)) sc1) sc1)
                (syntax 'x (seteq sc1)))
  
  (check-equal? (flip-scope (syntax 'x (seteq sc1)) sc2)
                (syntax 'x (seteq sc1 sc2)))
  (check-equal? (flip-scope (syntax 'x (seteq sc1 sc2)) sc2)
                (syntax 'x (seteq sc1))))

;; ----------------------------------------
;; Global binding table

;; A binding is either a gensym for a local variable or a
;; symbol for a core form or primitive

;; Global table of bindings
(define all-bindings (make-hash))

(define (add-binding! id binding)
  (hash-set! all-bindings id binding))

(module+ test
  (define loc1 (gensym 'loc))
  (define loc2 (gensym 'loc))
  
  ;; Same binding in  sc1  or  sc1 + sc2:
  (add-binding! (syntax 'test-a (seteq sc1)) loc1)

  ;; Shadowing in sc1 + sc2:
  (add-binding! (syntax 'test-b (seteq sc1)) loc1)
  (add-binding! (syntax 'test-b (seteq sc1 sc2)) loc2)

  ;; Ambiguous in sc1 + sc2:
  (add-binding! (syntax 'test-c (seteq sc1)) loc1)
  (add-binding! (syntax 'test-c (seteq sc2)) loc2))

;; Finds the binding for a given identifier; returns #f if the
;; identifier is unbound
(define (resolve id)
  (define candidate-ids (find-all-matching-bindings id))
  (cond
   [(pair? candidate-ids)
    (define max-candidate-id
      (argmax (lambda (c-id) (set-count (syntax-scopes c-id)))
              candidate-ids))
    (check-unambiguous max-candidate-id candidate-ids id)
    (hash-ref all-bindings max-candidate-id)]
   [else #f]))

(module+ test
  (check-equal? (resolve (syntax 'test-a (seteq sc1)))
                loc1)
  (check-equal? (resolve (syntax 'test-a (seteq sc1 sc2)))
                loc1)
  (check-equal? (resolve (syntax 'test-a (seteq sc2)))
                #f)

  (check-equal? (resolve (syntax 'test-b (seteq sc1)))
                loc1)
  (check-equal? (resolve (syntax 'test-b (seteq sc1 sc2)))
                loc2)
  (check-equal? (resolve (syntax 'test-b (seteq sc2)))
                #f)

  (check-equal? (resolve (syntax 'test-c (seteq sc1)))
                loc1)
  (check-exn (make-exn:fail? "ambiguous")
             (lambda () (resolve (syntax 'test-c (seteq sc1 sc2)))))
  (check-equal? (resolve (syntax 'test-c (seteq sc2)))
                loc2))

;; Find all candidiate bindings for `id` as the ones with
;; a subset of the scopes of `id`
(define (find-all-matching-bindings id)
  (for/list ([(c-id) (in-hash-keys all-bindings)]
             #:when (and (eq? (syntax-e c-id) (syntax-e id))
                         (subset? (syntax-scopes c-id) (syntax-scopes id))))
    c-id))

(module+ test
  (check-equal? (find-all-matching-bindings (syntax 'test-a (seteq sc1)))
                (list (syntax 'test-a (seteq sc1))))
  (check-equal? (find-all-matching-bindings (syntax 'test-a (seteq sc2)))
                (list))
  
  (check-equal? (list->set (find-all-matching-bindings (syntax 'test-b (seteq sc1 sc2))))
                (set (syntax 'test-b (seteq sc1))
                     (syntax 'test-b (seteq sc1 sc2))))
  
  (check-equal? (list->set (find-all-matching-bindings (syntax 'test-c (seteq sc1 sc2))))
                (set (syntax 'test-c (seteq sc1))
                     (syntax 'test-c (seteq sc2)))))

;; Check that the binding with the biggest scope set is a superset
;; of all the others
(define (check-unambiguous max-candidate-id candidate-ids error-id)
  (for ([c-id (in-list candidate-ids)])
    (unless (subset? (syntax-scopes c-id)
                     (syntax-scopes max-candidate-id))
      (error "ambiguous:" error-id))))

(module+ test
  (check-equal? (check-unambiguous (syntax 'test-b (seteq sc1 sc2))
                                   (list (syntax 'test-b (seteq sc1))
                                         (syntax 'test-b (seteq sc1 sc2)))
                                   (syntax 'test-b (seteq sc1 sc2)))
                (void))
  (check-exn (make-exn:fail? "ambiguous")
             (lambda ()
               (check-unambiguous (syntax 'test-c (seteq sc2))
                                  (list (syntax 'test-c (seteq sc1))
                                        (syntax 'test-c (seteq sc2)))
                                  (syntax 'test-c (seteq sc1 sc2))))))
  
;; Determine whether two identifiers have the same binding
(define (free-identifier=? a b)
  (eq? (resolve a) (resolve b)))

(module+ test
  (check-equal? (free-identifier=? (syntax 'test-c (seteq sc2))
                                   (syntax 'test-b (seteq sc1 sc2)))
                #t)
  (check-equal? (free-identifier=? (syntax 'test-a (seteq sc1))
                                   (syntax 'test-b (seteq sc1 sc2)))
                #f))               

;; ----------------------------------------
;; Compile-time environment

;; An expansion environment maps a local-binding gensym to a procedure
;; for a macro or the constant `variable` for a run-time variable
(define empty-env (hasheq))
(define variable (gensym 'variable))

;; The `env-lookup` function reports the constant `missing` if
;; a value is not found for a key
(define missing (gensym 'missing))

(define (env-extend env key val)
  (hash-set env key val))

(define (env-lookup env binding)
  (hash-ref env binding missing))

(module+ test
  (check-equal? (env-lookup empty-env loc1)
                missing)
  (check-equal? (env-lookup (env-extend empty-env loc1 'variable)
                            loc1)
                'variable))

;; Helper for registering a local binding in a set of scopes,
;; returns the gensym created to represent the binding
(define (add-local-binding! id)
  (define key (gensym (syntax-e id)))
  (add-binding! id key)
  key)

(module+ test
  (define loc3 (add-local-binding! (syntax 'test-d (seteq sc1 sc2))))
  (check-equal? (resolve (syntax 'test-d (seteq sc1 sc2)))
                loc3))

;; ----------------------------------------
;; Core syntax and primitives

;; Accumulate all core bindings in `core-scope`, so we can
;; easily generate a reference to a core form using `core-stx`:
(define core-scope (scope))

(define core-forms (seteq 'lambda 'let-syntax '#%app 'quote 'quote-syntax))
(define core-primitives (seteq 'datum->syntax 'syntax-e 'list 'cons 'car 'cdr 'map))

;; Bind core forms and primitives:
(for ([sym (in-set (set-union core-forms core-primitives))])
  (add-binding! (syntax sym (seteq core-scope)) sym))

;; The `namespace-syntax-introduce` function adds the core scope to a
;; syntax object; it needs to be used, for example, on a just-created
;; syntax object to make `lambda` refer to the core lambda form
(define (namespace-syntax-introduce s)
  (add-scope s core-scope))

(module+ test
  (check-equal? (resolve (datum->syntax 'lambda))
                #f)
  (check-equal? (resolve (namespace-syntax-introduce (datum->syntax 'lambda)))
                'lambda)) ; i.e., the core `lambda` form

;; ----------------------------------------
;; Pattern matching

;; We use a simple form pattern matching in the implementation of the
;; expander (but it's just internal; we don't provide any
;; pattern-matching facilities to programs that use the xpander).

;; Define a matcher on syntax using "match.rkt":
(define match-syntax
  (make-match-syntax syntax? identifier? syntax-e))

(module+ test
  (define (get-head s)
    (define m (match-syntax s '(a b ...)))
    (m 'a))
  (define (get-tail s)
    (define m (match-syntax s '(a b ...)))
    (m 'b))
  
  (check-equal? (get-head (datum->syntax '(a b c)))
                (syntax 'a (seteq)))
  (check-equal? (get-tail (datum->syntax '(a b c)))
                (list (syntax 'b (seteq))
                      (syntax 'c (seteq)))))

;; ----------------------------------------
;; The expander

(module+ test
  ;; Examples to demonstrate `expand`
  
  ;; A `(lambda (x) x)` form expands to itself, as long as it has the scope
  ;; used to binds all core-forms:
  (check-equal? (syntax->datum
                 (expand (namespace-syntax-introduce
                          (datum->syntax '(lambda (x) x)))
                         empty-env))
                '(lambda (x) x))
  
  ;; A reference to a core primitive expands to itself:
  (check-equal? (expand (syntax 'cons (seteq core-scope))
                        empty-env)
                (syntax 'cons (seteq core-scope)))
  
  ;; A locally-bound variable expands to itself:
  (check-equal? (expand (syntax 'test-a (seteq sc1)) ; bound to `loc1` above
                        (env-extend empty-env loc1 variable))
                (syntax 'test-a (seteq sc1)))
  
  ;; A free variable triggers an error:
  (check-exn (make-exn:fail? "free variable")
             (lambda ()
               (expand (syntax 'test-a (seteq))
                       empty-env)))
  
  ;; A number expands to a `quote` form:
  (check-equal? (expand (datum->syntax 1) empty-env)
                (syntax (list (syntax 'quote (seteq core-scope))
                              (syntax 1 (seteq)))
                        (seteq)))
  
  ;; Application of a locally-bound variable to a number expands to an
  ;; `#%app` form:
  (check-equal? (expand (syntax (list (syntax 'test-a (seteq sc1))
                                      (syntax 1 (seteq)))
                                (seteq))
                        (env-extend empty-env loc1 variable))
                (syntax (list (syntax '#%app (seteq core-scope))
                              (syntax 'test-a (seteq sc1))
                              (syntax (list (syntax 'quote (seteq core-scope))
                                            (syntax 1 (seteq)))
                                      (seteq)))
                        (seteq)))

  ;; Application of a number to a number expands to an `#%app` form
  ;; (but will be a run-time error if evaluated):
  (check-equal? (expand (datum->syntax '(0 1))
                        empty-env)
                (syntax (list (syntax '#%app (seteq core-scope))
                              (syntax (list (syntax 'quote (seteq core-scope))
                                            (syntax 0 (seteq)))
                                      (seteq))
                              (syntax (list (syntax 'quote (seteq core-scope))
                                            (syntax 1 (seteq)))
                                      (seteq)))
                        (seteq)))
  
  ;; A locally-bound macro expands by applying the macro:
  (check-equal? (syntax->datum
                 (expand (syntax 'test-a (seteq sc1))
                         (env-extend empty-env loc1 (lambda (s) (datum->syntax 1)))))
                '(quote 1))
  (check-equal? (syntax->datum
                 (expand (let ([s (datum->syntax '(test-a (lambda (x) x)))])
                           (add-scope (add-scope s sc1) core-scope))
                         (env-extend empty-env loc1 (lambda (s) (list-ref (syntax-e s) 1)))))
                '(lambda (x) x)))

;; Main expander entry point and loop:
(define (expand s [env empty-env])
  (cond
   [(identifier? s)
    (expand-identifier s env)]
   [(and (pair? (syntax-e s))
         (identifier? (car (syntax-e s))))
    (expand-id-application-form s env)]
   [(or (pair? (syntax-e s))
        (null? (syntax-e s)))
    ;; An application form that doesn't start with an identifier
    (expand-app s env)]
   [else
    ;; Anything other than an identifier or parens is implicitly quoted,
    ;; so build a `quote` form
    (syntax (list (syntax 'quote (seteq core-scope))
                  s)
            (seteq))]))

;; An identifier by itself:
(define (expand-identifier s env)
  (define binding (resolve s))
  (cond
   [(not binding)
    (error "free variable:" s)]
   [(set-member? core-forms binding)
    (error "bad syntax:" s)]
   [(set-member? core-primitives binding)
    s]
   [else
    (define v (env-lookup env binding))
    (cond
     [(eq? v missing)
      (error "out of context:" s)]
     [(eq? v variable)
      s]
     [(procedure? v)
      ;; Apply a macro, then recur:
      (expand (apply-transformer v s) env)]
     [else
      ;; Compile-time value that's not a procedure
      (error "illegal use of syntax:" s)])]))

;; An "application" form that starts with an identifier
(define (expand-id-application-form s env)
  (define id (car (syntax-e s)))
  (define binding (resolve id))
  (case binding
    [(lambda)
     (expand-lambda s env)]
    [(let-syntax)
     (expand-let-syntax s env)]
    [(#%app)
     (define m (match-syntax s '(#%app e ...)))
     (expand-app (syntax (m 'e) (seteq)) env)]
    [(quote quote-syntax)
     s]
    [else
     (define v (env-lookup env binding))
     (cond
      [(procedure? v)
       ;; Apply transformer, then recur
       (expand (apply-transformer v s) env)]
      [else
       (expand-app s env)])]))

;; Given a macro transformer `t`, apply it --- adding appropriate
;; scopes to represent the expansion step
(define (apply-transformer t s)
  (define intro-scope (scope))
  (define intro-s (add-scope s intro-scope))
  ;; Call the transformer
  (define transformed-s (t intro-s))
  ;; Flip intro scope to get final result:
  (flip-scope transformed-s intro-scope))

(module+ test
  ;; Check that applying a macro transformer adds a scope to
  ;; introduced parts and leaves original parts alone:
  (define transformed-s 
    (apply-transformer
     (lambda (s)
       ;; This transformer converts `(_ f)` to `(f x)`
       (syntax (list (list-ref (syntax-e s) 1)
                     (syntax 'x (seteq)))
               (seteq)))
     (syntax (list (syntax 'm (seteq))
                   (syntax 'f (seteq sc1)))
             (seteq))))
  (check-equal? (syntax->datum transformed-s)
                '(f x))
  (check-equal? (list-ref (syntax-e transformed-s) 0)
                (syntax 'f (seteq sc1)))
  (check-equal? (set-count (syntax-scopes
                            (list-ref (syntax-e transformed-s) 1)))
                1))

;; ----------------------------------------

(define (expand-lambda s env)
  (define m (match-syntax s '(lambda (id ...) body)))
  (define sc (scope))
  ;; Add the new scope to each binding identifier:
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
  (syntax (list (m 'lambda) (syntax ids (seteq)) exp-body)
          (seteq)))

(define (expand-let-syntax s env)
  (define m (match-syntax s '(let-syntax ([trans-id trans-rhs]
                                          ...)
                              body)))
  (define sc (scope))
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
  (expand (add-scope (m 'body) sc) body-env))

;; Expand an application (i.e., a function call)
(define (expand-app s env)
  (define m (match-syntax s '(rator rand ...)))
  ;; Add `#%app` to make the application form explicit
  (syntax
   (list* (syntax '#%app (seteq core-scope))
          (expand (m 'rator) env)
          (for/list ([e (in-list (m 'rand))])
            (expand e env)))
   (seteq)))

;; ----------------------------------------

;; Expand and evaluate `rhs` as a compile-time expression
(define (eval-for-syntax-binding rhs env)
  (eval-compiled (compile (expand rhs empty-env))))

(module+ test
  (check-equal? (eval-for-syntax-binding (add-scope (datum->syntax
                                                     '(car (list 1 2)))
                                                    core-scope)
                                         empty-env)
                1)
  
  (check-equal? ((eval-for-syntax-binding (add-scope (datum->syntax
                                                      '(lambda (x) (syntax-e x)))
                                                     core-scope)
                                          empty-env)
                 (syntax 'x (seteq)))
                'x))

;; ----------------------------------------

;; Convert an expanded syntax object to an expression that is
;; represented by a plain S-expression.
(define (compile s)
  (cond
   [(pair? (syntax-e s))
    (define core-sym (resolve (car (syntax-e s))))
    (case core-sym
      [(#f)
       (error "not a core form:" s)]
      [(lambda)
       (define m (match-syntax s '(lambda (id ...) body)))
       `(lambda ,(map resolve (m 'id)) ,(compile (m 'body)))]
      [(#%app)
       (define m (match-syntax s '(#%app . rest)))
       (for/list ([s (in-list (m 'rest))])
         (compile s))]
      [(quote)
       (define m (match-syntax s '(quote datum)))
       ;; Strip away scopes:
       `(quote ,(syntax->datum (m 'datum)))]
      [(quote-syntax)
       (define m (match-syntax s '(quote datum)))
       ;; Preserve the complete syntax object:
       `(quote ,(m 'datum))]
      [else
       (error "unrecognized core form:" core-sym)])]
   [(identifier? s)
    (resolve s)]
   [else
    (error "bad syntax after expansion:" s)]))

;; Using the host Racket system: create a fresh namespace for
;; evaluating expressions that have been `expand`ed and `compile`d,
;; and install the expander's `datum->syntax` and `syntax-e`
;; to replace the host primitives
(define namespace (make-base-namespace))
(namespace-set-variable-value! 'datum->syntax datum->syntax #t namespace)
(namespace-set-variable-value! 'syntax-e syntax-e #t namespace)

(define (eval-compiled s)
  (eval s namespace))
