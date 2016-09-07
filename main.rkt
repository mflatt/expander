#lang racket

(provide expand
         compile
         (rename-out [eval-compiled eval])

         datum->syntax
         introduce)

;; We include tests to serve as examples of various functions work
(module+ test
  (require rackunit)
  (define (make-exn:fail? rx)
    (lambda (v)
      (and (exn:fail? v) (regexp-match? rx (exn-message v))))))

;; ----------------------------------------
;; Syntax objects

(struct syntax (e        ; a symbol
                scopes)  ; a set of scopes
        #:transparent)

;; For now, all syntax object are identifiers:
(define (identifier? s)
  (syntax? s))

(module+ test
  (check-equal? (identifier? (syntax 'x (seteq)))
                #t))

;; The `datum->syntax` function coerces to syntax with an empty scope
;; set, leaving existing syntax as-is
(define (datum->syntax v)
  (cond
   [(syntax? v) v]
   [(symbol? v) (syntax v (seteq))]
   [(list? v) (map datum->syntax v)]
   [else v]))

(module+ test
  (check-equal? (datum->syntax 'a)
                (syntax 'a (seteq)))
  (check-equal? (datum->syntax 1)
                1)
  (check-equal? (datum->syntax '(a b c))
                (list (syntax 'a (seteq))
                      (syntax 'b (seteq))
                      (syntax 'c (seteq))))
  (check-equal? (datum->syntax (list 'a
                                     (syntax 'b (seteq))
                                     'c))
                (list (syntax 'a (seteq))
                      (syntax 'b (seteq))
                      (syntax 'c (seteq)))))

;; The `syntax->datum` function discards scopes --- immediate and
;; nested --- to produce a plain S-expression:
(define (syntax->datum s)
  (cond
   [(syntax? s) (syntax-e s)]
   [(list? s) (map syntax->datum s)]
   [else s]))

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

(module+ test
  (define sc1 (scope))
  (define sc2 (scope))
  
  (check-equal? (eq? sc1 sc2) #f)
  (check-equal? (eq? sc1 sc1) #t))

;; Add or flip a scope everywhere (i.e., including nested syntax)
(define (adjust-scope s sc op)
  (cond
   [(syntax? s) (syntax (syntax-e s)
                        (op (syntax-scopes s) sc))]
   [(list? s) (map (lambda (e) (adjust-scope e sc op))
                   s)]
   [else s]))

(define (add-scope s sc)
  (adjust-scope s sc set-add))

(define (flip-scope s sc)
  (adjust-scope s sc set-flip))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(module+ test
  (check-equal? (add-scope (syntax 'x (seteq)) sc1)
                (syntax 'x (seteq sc1)))
  (check-equal? (add-scope (datum->syntax '(x (y))) sc1)
                (list (syntax 'x (seteq sc1))
                      (list (syntax 'y (seteq sc1)))))
  
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
  (define loc/a (gensym 'a))
  ;; Simulates
  ;;   (let ([a 1])
  ;;     (let ([z 2])
  ;;       ....))
  ;; where `a` is bound only once

  (define loc/b-out (gensym 'b))
  (define loc/b-in (gensym 'b))
  ;; Simulates
  ;;   (let ([b 1])
  ;;     (let ([b 2])
  ;;       ....))
  ;; where the inner `b` shadows the outer `b`
  
  (define loc/c1 (gensym 'c))
  (define loc/c2 (gensym 'c))
  ;; Simulates
  ;;    (list (let ([c 1]) ...)
  ;;          (let ([c 2]) ...)))
  ;; where the `c`s have non-overlaping binding scopes
  
  ;; Same binding in  sc1  or  sc1 + sc2:
  (add-binding! (syntax 'a (seteq sc1)) loc/a)

  ;; Shadowing in sc1 + sc2:
  (add-binding! (syntax 'b (seteq sc1)) loc/b-out)
  (add-binding! (syntax 'b (seteq sc1 sc2)) loc/b-in)
  
  ;; Ambiguous in sc1 + sc2:
  (add-binding! (syntax 'c (seteq sc1)) loc/c1)
  (add-binding! (syntax 'c (seteq sc2)) loc/c2))

;; Finds the binding for a given identifier; returns #f if the
;; identifier is unbound
(define (resolve id)
  (define candidate-ids (find-all-matching-bindings id))
  (cond
   [(pair? candidate-ids)
    (define max-id
      (argmax (compose set-count syntax-scopes)
              candidate-ids))
    (check-unambiguous max-id candidate-ids)
    (hash-ref all-bindings max-id)]
   [else #f]))

(module+ test
  (check-equal? (resolve (syntax 'a (seteq sc1)))
                loc/a)
  (check-equal? (resolve (syntax 'a (seteq sc1 sc2)))
                loc/a)
  (check-equal? (resolve (syntax 'a (seteq sc2)))
                #f)

  (check-equal? (resolve (syntax 'b (seteq sc1)))
                loc/b-out)
  (check-equal? (resolve (syntax 'b (seteq sc1 sc2)))
                loc/b-in)
  (check-equal? (resolve (syntax 'b (seteq sc2)))
                #f)

  (check-equal? (resolve (syntax 'c (seteq sc1)))
                loc/c1)
  (check-exn (make-exn:fail? "ambiguous")
             (lambda () (resolve (syntax 'c (seteq sc1 sc2)))))
  (check-equal? (resolve (syntax 'c (seteq sc2)))
                loc/c2))

;; Find all candidiate bindings for `id` as the ones with
;; a subset of the scopes of `id`
(define (find-all-matching-bindings id)
  (for/list ([c-id (in-hash-keys all-bindings)]
             #:when (and (eq? (syntax-e c-id) (syntax-e id))
                         (subset? (syntax-scopes c-id) (syntax-scopes id))))
    c-id))

(module+ test
  (check-equal? (find-all-matching-bindings (syntax 'a (seteq sc1)))
                (list (syntax 'a (seteq sc1))))
  (check-equal? (find-all-matching-bindings (syntax 'a (seteq sc2)))
                (list))
  
  (check-equal? (list->set (find-all-matching-bindings (syntax 'b (seteq sc1 sc2))))
                (set (syntax 'b (seteq sc1))
                     (syntax 'b (seteq sc1 sc2))))
  
  (check-equal? (list->set (find-all-matching-bindings (syntax 'c (seteq sc1 sc2))))
                (set (syntax 'c (seteq sc1))
                     (syntax 'c (seteq sc2)))))

;; Check that the binding with the biggest scope set is a superset
;; of all the others
(define (check-unambiguous max-id candidate-ids)
  (for ([c-id (in-list candidate-ids)])
    (unless (subset? (syntax-scopes c-id)
                     (syntax-scopes max-id))
      (error "ambiguous:" max-id))))

(module+ test
  (check-equal? (check-unambiguous (syntax 'b (seteq sc1 sc2))
                                   (list (syntax 'b (seteq sc1))
                                         (syntax 'b (seteq sc1 sc2))))
                (void))
  (check-exn (make-exn:fail? "ambiguous")
             (lambda ()
               (check-unambiguous (syntax 'c (seteq sc2))
                                  (list (syntax 'c (seteq sc1))
                                        (syntax 'c (seteq sc2)))))))
  
;; ----------------------------------------
;; Core syntax and primitives

;; Accumulate all core bindings in `core-scope`
(define core-scope (scope))

(define core-forms (seteq 'lambda 'let-syntax 'quote 'quote-syntax))
(define core-primitives (seteq 'datum->syntax 'syntax->datum 'syntax-e
                               'list 'cons 'car 'cdr 'map))

;; Bind core forms and primitives:
(for ([sym (in-set (set-union core-forms core-primitives))])
  (add-binding! (syntax sym (seteq core-scope)) sym))

;; The `introduce` function adds the core scope to a
;; syntax object; it needs to be used, for example, on a just-created
;; syntax object to make `lambda` refer to the core lambda form
(define (introduce s)
  (add-scope s core-scope))

(module+ test
  (check-equal? (resolve (datum->syntax 'lambda))
                #f)
  (check-equal? (resolve (introduce (datum->syntax 'lambda)))
                'lambda)) ; i.e., the core `lambda` form

;; ----------------------------------------
;; Compile-time environment

;; An expansion environment maps a local-binding gensym to a procedure
;; for a macro or the constant `variable` for a run-time variable
(define empty-env (hasheq))
(define variable (gensym 'variable))

(define (env-extend env key val)
  (hash-set env key val))

;; The `env-lookup` function reports #f if
;; a value is not found for a key
(define (env-lookup env binding)
  (hash-ref env binding #f))

(module+ test
  (check-equal? (env-lookup empty-env loc/a)
                #f)
  (check-equal? (env-lookup (env-extend empty-env loc/a 'variable)
                            loc/a)
                'variable))

;; Helper for registering a local binding in a set of scopes,
;; returns the gensym created to represent the binding
(define (add-local-binding! id)
  (define key (gensym (syntax-e id)))
  (add-binding! id key)
  key)

(module+ test
  (define loc/d (add-local-binding! (syntax 'd (seteq sc1 sc2))))
  (check-equal? (resolve (syntax 'd (seteq sc1 sc2)))
                loc/d))

;; ----------------------------------------
;; Expansion Dispatch

(module+ test
  ;; Examples to demonstrate `expand`

  ;; Binding and using a macro:
  (check-equal? (syntax->datum
                 (expand (add-scope
                          (datum->syntax
                           '(let-syntax ([one (lambda (stx)
                                                (quote-syntax '1))])
                             (one)))
                          core-scope)))
                '(quote 1))
  
  ;; A `(lambda (x) x)` form expands to itself, as long as it has the scope
  ;; used to bind all core-forms:
  (check-equal? (syntax->datum
                 (expand (add-scope (datum->syntax '(lambda (x) x)) core-scope)
                         empty-env))
                '(lambda (x) x))
  
  ;; A reference to a core primitive expands to itself:
  (check-equal? (expand (syntax 'cons (seteq core-scope))
                        empty-env)
                (syntax 'cons (seteq core-scope)))
  
  ;; A locally-bound variable expands to itself:
  (check-equal? (expand (syntax 'a (seteq sc1)) ; bound to `loc1` above
                        (env-extend empty-env loc/a variable))
                (syntax 'a (seteq sc1)))
  
  ;; A free variable triggers an error:
  (check-exn (make-exn:fail? "free variable")
             (lambda ()
               (expand (syntax 'a (seteq))
                       empty-env)))
  
  ;; Application of a locally-bound variable to a number quotes the
  ;; number argument expression:
  (check-equal? (expand (list (syntax 'a (seteq sc1))
                              (list (syntax 'quote (seteq core-scope)) 1))
                        (env-extend empty-env loc/a variable))
                (list (syntax 'a (seteq sc1))
                      (list (syntax 'quote (seteq core-scope))
                            1)))

  ;; Application of a number to a number expands to an application
  ;; (but will be a run-time error if evaluated):
  (check-equal? (expand (introduce
                         (datum->syntax '('0 '1)))
                        empty-env)
                (list (list (syntax 'quote (seteq core-scope))
                            0)
                      (list (syntax 'quote (seteq core-scope))
                            1)))
  
  ;; A locally-bound macro expands by applying the macro:
  (check-equal? (syntax->datum
                 (expand (list (syntax 'a (seteq sc1)))
                         (env-extend empty-env loc/a (lambda (s) (list (syntax 'quote (seteq core-scope))
                                                                  1)))))
                '(quote 1))
  (check-equal? (syntax->datum
                 (expand (let ([s (datum->syntax '(a (lambda (x) x)))])
                           (add-scope (add-scope s sc1) core-scope))
                         (env-extend empty-env loc/a (lambda (s) (list-ref s 1)))))
                '(lambda (x) x)))

;; Main expander entry point and loop:
(define (expand s [env empty-env])
  (cond
   [(identifier? s)
    ;; An identifier by itself
    (expand-identifier s env)]
   [(and (pair? s)
         (identifier? (car s)))
    ;; An "application" of an identifier; maybe a form or macro
    (expand-id-application-form s env)]
   [(or (pair? s)
        (null? s))
    ;; An application form that doesn't start with an identifier
    (expand-app s env)]
   [else
    (error "bad syntax:" s)]))

;; An identifier by itself:
(define (expand-identifier s env)
  (define binding (resolve s))
  (cond
   [(not binding)
    (error "free variable:" s)]
   [(set-member? core-primitives binding)
    s]
   [(set-member? core-forms binding)
    (error "bad syntax:" s)]
   [else
    (define v (env-lookup env binding))
    (cond
     [(eq? v variable)
      s]
     [(not v)
      (error "out of context:" s)]
     [else
      (error "bad syntax:" s)])]))

;; An "application" form that starts with an identifier
(define (expand-id-application-form s env)
  (define id (car s))
  (define binding (resolve id))
  (case binding
    [(lambda)
     (expand-lambda s env)]
    [(let-syntax)
     (expand-let-syntax s env)]
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
       (list (list-ref s 1)
             (syntax 'x (seteq))))
     (list (syntax 'm (seteq))
           (syntax 'f (seteq sc1)))))
  (check-equal? (syntax->datum transformed-s)
                '(f x))
  (check-equal? (list-ref transformed-s 0)
                (syntax 'f (seteq sc1)))
  (check-equal? (set-count (syntax-scopes
                            (list-ref transformed-s 1)))
                1))

;; ----------------------------------------

(define (expand-lambda s env)
  (match-define `(,lambda-id (,arg-id) ,body) s)
  (define sc (scope))
  ;; Add the new scope to the binding identifier:
  (define id (add-scope arg-id sc))
  ;; Bind the argument and generate a corresponding key for the
  ;; expand-time environment:
  (define binding (add-local-binding! id))
  (define body-env (env-extend env binding variable))
  ;; Expand the function body:
  (define exp-body (expand (add-scope body sc) body-env))
  (list lambda-id (list id) exp-body))

(define (expand-let-syntax s env)
  (match-define `(,let-syntax-id ([,lhs-id ,rhs])
                    ,body)
                s)
  (define sc (scope))
  ;; Add the new scope to each binding identifier:
  (define id (add-scope lhs-id sc))
  ;; Bind the left-hand identifier and generate a corresponding key
  ;; for the expand-time environment:
  (define binding (add-local-binding! id))
  ;; Evaluate compile-time expressions:
  (define rhs-val (eval-for-syntax-binding rhs))
  ;; Fill expansion-time environment:
  (define body-env (env-extend env binding rhs-val))
  ;; Expand body
  (expand (add-scope body sc) body-env))

;; Expand an application (i.e., a function call)
(define (expand-app s env)
  (map (lambda (sub-s) (expand sub-s env)) s))

;; ----------------------------------------

;; Expand and evaluate `rhs` as a compile-time expression
(define (eval-for-syntax-binding rhs)
  (eval-compiled (compile (expand rhs empty-env))))

(module+ test
  (check-equal? (eval-for-syntax-binding (add-scope (datum->syntax
                                                     '(car (list '1 '2)))
                                                    core-scope))
                1)
  
  (check-equal? ((eval-for-syntax-binding (add-scope (datum->syntax
                                                      '(lambda (x) (syntax-e x)))
                                                     core-scope))
                 (syntax 'x (seteq)))
                'x))

;; ----------------------------------------

;; Convert an expanded syntax object to an expression that is
;; represented by a plain S-expression.
(define (compile s)
  (cond
   [(pair? s)
    (define core-sym (and (identifier? (car s))
                          (resolve (car s))))
    (case core-sym
      [(lambda)
       (match-define `(,lambda-id (,id) ,body) s)
       `(lambda (,(resolve id)) ,(compile body))]
      [(quote)
       (match-define `(,quote-id ,datum) s)
       ;; Strip away scopes:
       `(quote ,(syntax->datum datum))]
      [(quote-syntax)
       (match-define `(,quote-syntax-id ,datum) s)
       ;; Preserve the complete syntax object:
       `(quote ,datum)]
      [else
       ;; Application:
       (map compile s)])]
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
(namespace-set-variable-value! 'syntax->datum syntax->datum #t namespace)
(namespace-set-variable-value! 'syntax-e syntax-e #t namespace)

(define (eval-compiled s)
  (eval s namespace))
