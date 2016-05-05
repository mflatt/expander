#lang racket
(require "match.rkt")

(provide expand
         compile
         (rename-out [eval-compiled eval])

         datum->syntax
         namespace-syntax-introduce)

;; ----------------------------------------

(struct syntax (e        ; datum and nested syntax objects
                scopes)  ; scopes that apply at all phases
        #:transparent)

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-e s))))

;; Discard scopes:
(define (syntax->datum s)
  (let loop ([s (syntax-e s)])
    (cond
     [(syntax? s) (loop (syntax-e s))]
     [(pair? s) (cons (loop (car s))
                      (loop (cdr s)))]
     [else s])))

;; Coerce to syntax, adding an empty scope set:
(define (datum->syntax s)
  (cond
   [(syntax? s) s]
   [(list? s) (syntax (map datum->syntax s)
                      (seteq))]
   [(pair? s) (syntax (cons (datum->syntax (car s))
                            (datum->syntax (cdr s)))
                      (seteq))]
   [else (syntax s (seteq))]))

;; Define a matcher using "match.rkt":
(define match-syntax
  (make-match-syntax syntax? identifier? syntax-e))

;; ----------------------------------------

;; A scope's identity is based on `eq?`
(struct scope ())

;; Add or flip a scope --- recurs to nested syntax
(define (apply-scope s sc op)
  (cond
   [(syntax? s) (struct-copy syntax s
                             [e (apply-scope (syntax-e s) sc op)]
                             [scopes (op (syntax-scopes s) sc)])]
   [(pair? s) (cons (apply-scope (car s) sc op)
                    (apply-scope (cdr s) sc op))]
   [else s]))

(define (add-scope s sc)
  (apply-scope s sc set-add))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (apply-scope s sc set-flip))

;; ----------------------------------------

;; Global table of bindings
(define all-bindings (make-hash))

(define (add-binding! id binding)
  (hash-set! all-bindings id binding))

(define (resolve id)
  (define candidate-ids
    (for/list ([(c-id) (in-hash-keys all-bindings)]
               #:when (and (eq? (syntax-e c-id) (syntax-e id))
                           (subset? (syntax-scopes c-id) (syntax-scopes id))))
      c-id))
  (define max-candidate-id
    (and (pair? candidate-ids)
         (car
          (sort candidate-ids >
                #:key (lambda (c-id) (set-count (syntax-scopes c-id)))))))
  (cond
   [max-candidate-id
    (for ([c-id (in-list candidate-ids)])
      (unless (subset? (syntax-scopes c-id)
                       (syntax-scopes max-candidate-id))
        (error "ambiguous:" id)))
    (hash-ref all-bindings max-candidate-id)]
   [else #f]))

;; ----------------------------------------

;; A binding is either a gensym for a local variable or a
;; symbol for a core form or primitive

(define (free-identifier=? a b)
  (eq? (resolve a) (resolve b)))

;; ----------------------------------------

;; An expansion environment maps keys to a generated symbol
;; for a variable or a procedure for a macro:
(define empty-env #hasheq())
(define variable (gensym 'variable))

(define (env-extend env key val)
  (hash-set env key val))

(define unbound (gensym 'unbound))
(define (env-lookup env binding)
  (hash-ref env binding unbound))

;; Helper for registering a local binding in a set of scopes:
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


;; ----------------------------------------

(define (expand s [env empty-env])
  (cond
   [(identifier? s)
    ;; An identifier by itself
    (define binding (resolve s))
    (cond
     [(set-member? core-forms binding)
      (error "bad syntax:" s)]
     [(set-member? core-primitives binding)
      s]
     [else
      (define v (env-lookup env binding))
      (cond
       [(eq? v unbound)
        (error "free variable:" s)]
       [(eq? v variable)
        s]
       [(procedure? v)
        ;; apply a macro, then recur:
        (expand (v s) env)]
       [else
        ;; compile-time value that's not a procedure
        (error "illegal use of syntax:" s)])])]
   [(and (pair? (syntax-e s))
         (identifier? (car (syntax-e s))))
    ;; An "application" form that starts with an identifier
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
         ;; apply a macro, then recur:
         (expand (v s) env)]
        [else
         (expand-app s env)])])]
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
  (syntax (list (m 'lambda) ids exp-body)
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

;; Expand and evaluate `s` as a compile-time expression
(define (eval-for-syntax-binding rhs env)
  (eval-compiled (compile (expand rhs empty-env))))

;; ----------------------------------------

;; Convert an expanded syntax object to an expression that is represented
;; by a plain S-expression.
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

(define namespace (make-base-namespace))
(namespace-set-variable-value! 'datum->syntax datum->syntax #t namespace)
(namespace-set-variable-value! 'syntax-e syntax-e #t namespace)

(define (eval-compiled s)
  (eval s namespace))

;; ----------------------------------------

(define (namespace-syntax-introduce s)
  ;; The only initial bindings are in the core scope
  (add-scope s core-scope))
