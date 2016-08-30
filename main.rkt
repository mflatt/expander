#lang racket
(require "match.rkt")

(provide expand
         compile
         (rename-out [eval-compiled eval])

         datum->syntax
         namespace-syntax-introduce)

;; ----------------------------------------

(struct syntax (e        ; a non-pair atom or a list of syntax
                scopes)  ; a set of scopes
        #:transparent)

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-e s))))

(define (bound-identifier=? a b)
  (and (eq? (syntax-e a) (syntax-e b))
       (equal? (syntax-scopes a) (syntax-scopes b))))

;; Discard scopes --- immediate and nested:
(define (syntax->datum s)
  (let ([e (syntax-e s)])
    (cond
     [(list? e) (map syntax->datum e)]
     [else e])))

;; Coerce to syntax with an empty scope set, leaving
;; existing syntax as-is
(define (datum->syntax v)
  (cond
   [(syntax? v) v]
   [(list? v) (syntax (map datum->syntax v)
                      (seteq))]
   [else (syntax v (seteq))]))

;; ----------------------------------------

;; A scope's identity is based on `eq?`
(struct scope ())

;; Add or flip a scope everywehere (i.e., including nested syntax)
(define (apply-scope s/e sc op)
  (cond
   [(syntax? s/e) (struct-copy syntax s/e
                               [e (apply-scope (syntax-e s/e) sc op)]
                               [scopes (op (syntax-scopes s/e) sc)])]
   [(list? s/e) (map (lambda (s) (apply-scope s sc op)) s/e)]
   [else s/e]))

(define (add-scope s sc)
  (apply-scope s sc set-add))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (apply-scope s sc set-flip))

;; ----------------------------------------

;; A binding is either a gensym for a local variable or a
;; symbol for a core form or primitive

;; Global table of bindings
(define all-bindings (make-hash))

(define (add-binding! id binding)
  (hash-set! all-bindings id binding))

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

;; Find all candidiate bindings for `id` as the ones with
;; a subset of the scopes of `id`
(define (find-all-matching-bindings id)
  (for/list ([(c-id) (in-hash-keys all-bindings)]
             #:when (and (eq? (syntax-e c-id) (syntax-e id))
                         (subset? (syntax-scopes c-id) (syntax-scopes id))))
    c-id))

;; Check that the binding with the biggest scope set is a superset
;; of all the others
(define (check-unambiguous max-candidate-id candidate-ids error-id)
  (for ([c-id (in-list candidate-ids)])
    (unless (subset? (syntax-scopes c-id)
                     (syntax-scopes max-candidate-id))
      (error "ambiguous:" error-id))))

(define (free-identifier=? a b)
  (eq? (resolve a) (resolve b)))

;; ----------------------------------------

;; An expansion environment maps a local-binding gensym to a procedure
;; for a macro or the constant `variable` for a run-time variable
(define empty-env (hasheq))
(define variable (gensym 'variable))

(define (env-extend env key val)
  (hash-set env key val))

(define missing (gensym 'missing))
(define (env-lookup env binding)
  (hash-ref env binding missing))

;; Helper for registering a local binding in a set of scopes,
;; returns the gensym created to represent the binding
(define (add-local-binding! id)
  (define key (gensym (syntax-e id)))
  (add-binding! id key)
  key)

;; ----------------------------------------

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

;; ----------------------------------------

;; Define a matcher on syntax using "match.rkt":
(define match-syntax
  (make-match-syntax syntax? identifier? syntax-e))

;; ----------------------------------------

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
     (define m (match-syntax '(#%app e ...)))
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
