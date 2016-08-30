#lang racket/base
(require racket/set
         racket/unit
         "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "binding.rkt"
         "compile.rkt"
         "core.rkt")

(provide expand
         lookup
         eval-for-syntax-binding
         rebuild)

;; ----------------------------------------

;; Main expander loop:
(define (expand s env)
  (cond
   [(identifier? s)
    (expand-identifier s env)]
   [(and (pair? (syntax-e s))
         (identifier? (car (syntax-e s))))
    (expand-id-application-form s env)]
   [(or (pair? (syntax-e s))
        (null? (syntax-e s)))
    ;; An "application" form that doesn't start with an identifier
    (expand-app s env)]
   [else
    ;; Anything other than an identifier or parens is implicitly quoted
    (rebuild s (list (datum->syntax core-stx 'quote) s))]))

;; An identifier by itself:
(define (expand-identifier s env)
  (define binding (resolve s))
  (cond
   [(not binding)
    (error "unbound identifier:" s)]
   [else
    ;; Variable or form as identifier macro
    (dispatch (lookup binding env s) s env)]))

;; An "application" form that starts with an identifier
(define (expand-id-application-form s env)
  (define id (car (syntax-e s)))
  (define binding (resolve id))
  (define t (if binding
                (lookup binding env id)
                missing))
  ;; Find out whether it's bound as a variable, syntax, or core form
  (cond
   [(or (variable? t) (missing? t))
    (expand-app s env)]
   [else
    ;; Syntax or core form as "application"
    (dispatch t s env)]))

;; Expand `s` given that the value `t` of the relevant binding,
;; where `t` is either a core form, a macro transformer, some
;; other compile-time value (which is an error), or a token
;; indicating that the binding is a run-time variable
(define (dispatch t s env)
  (cond
   [(core-form? t)
    ((core-form-expander t) s env)]
   [(transformer? t)
    ;; Apply transformer and expand again
    (expand (apply-transformer t s) env)]
   [(variable? t)
    ;; A reference to a variable expands to itself
    s]
   [else
    ;; Some other compile-time value:
    (error "illegal use of syntax:" t)]))

;; Given a macro transformer `t`, apply it --- adding appropriate
;; scopes to represent the expansion step
(define (apply-transformer t s)
  (define intro-scope (new-scope))
  (define intro-s (add-scope s intro-scope))
  ;; Call the transformer
  (define transformed-s (t intro-s))
  (unless (syntax? transformed-s)
    (error "transformer produced non-syntax:" transformed-s))
  ;; Flip intro scope to get final result:
  (flip-scope transformed-s intro-scope))

;; Helper to lookup a binding with core forms
(define (lookup b env id)
  (binding-lookup b core-forms env id))

;; ----------------------------------------

;; Expand an application (i.e., a function call)
(define (expand-app s env)
  (define m (match-syntax s '(rator rand ...)))
  (rebuild
   s
   (list* (datum->syntax core-stx '#%app)
          (expand (m 'rator) env)
          (for/list ([e (in-list (m 'rand))])
            (expand e env)))))

;; ----------------------------------------

;; Expand `s` as a compile-time expression
(define (expand-transformer s env)
  (expand s empty-env))

;; Expand and evaluate `s` as a compile-time expression
(define (eval-for-syntax-binding rhs env)
  (define exp-rhs (expand-transformer rhs env))
  (expand-time-eval (compile exp-rhs)))

;; ----------------------------------------

;; A helper for forms to reconstruct syntax
(define (rebuild orig-s new)
  (datum->syntax orig-s new))
