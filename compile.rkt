#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "binding.rkt"
         "match.rkt"
         "core.rkt")

(provide compile
         expand-time-eval
         run-time-eval)

;; Convert an expanded syntax object to an expression that is represented
;; by a plain S-expression.
(define (compile s)
  (let ([compile (lambda (s) (compile s))])
    (cond
     [(pair? (syntax-e s))
      (define core-sym (core-form-sym s))
      (case core-sym
        [(#f)
         (error "not a core form:" s)]
        [(lambda)
         (define m (match-syntax s '(lambda formals body)))
         `(lambda ,@(compile-lambda (m 'formals) (m 'body)))]
        [(#%app)
         (define m (match-syntax s '(#%app . rest)))
         (for/list ([s (in-list (m 'rest))])
           (compile s))]
        [(let-values letrec-values)
         (compile-let core-sym s)]
        [(quote)
         (define m (match-syntax s '(quote datum)))
         `(quote ,(syntax->datum (m 'datum)))]
        [(quote-syntax)
         (define m (match-syntax s '(quote datum)))
         `(quote ,(m 'datum))]
        [else
         (error "unrecognized core form:" core-sym)])]
     [(identifier? s)
      (define b (resolve s))
      (cond
       [(local-binding? b)
        (define sym (key->symbol (local-binding-key b)))
        (unless sym
          (error "missing a binding after expansion:" s))
        sym]
       [(core-binding? b)
        (define sym (core-binding-sym b))
        (hash-ref core-primitives sym #f)]
       [else
        (error "not a reference to a local binding:" s)])]
     [else
      (error "bad syntax after expansion:" s)])))

(define (compile-lambda formals body)
  (define gen-formals
    (let loop ([formals formals])
      (cond
       [(identifier? formals) (local->symbol formals)]
       [(syntax? formals) (loop (syntax-e formals))]
       [(pair? formals) (cons (loop (car formals))
                              (loop (cdr formals)))]
       [else null])))
  `(,gen-formals ,(compile body)))

(define (compile-let core-sym s)
  (define rec? (eq? core-sym 'letrec-values))
  (define m (match-syntax s '(let-values ([(id ...) rhs] ...) body)))
  (define sc (new-scope))
  (define idss (m 'id))
  (define symss (for/list ([ids (in-list idss)])
                  (for/list ([id (in-list ids)])
                    (local->symbol id))))
  `(,core-sym ,(for/list ([syms (in-list symss)]
                          [rhs (in-list (m 'rhs))])
                 `[,syms ,(compile rhs)])
    ,(compile (m 'body))))

;; ----------------------------------------
         
(define (local->symbol id)
  (define b (resolve id))
  (unless (local-binding? b)
    (error "bad binding:" id))
  (key->symbol (local-binding-key b)))

(define (key->symbol key)
  ;; A local-binding key is already a symbol
  key)

;; ----------------------------------------

(define expand-time-namespace (make-base-namespace))
(define run-time-namespace (make-base-namespace))

(define (expand-time-eval compiled)
  (eval compiled expand-time-namespace))

(define (run-time-eval compiled)
  (eval compiled run-time-namespace))
