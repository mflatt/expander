#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "namespace.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "require.rkt"
         "compile.rkt")

;; ----------------------------------------

(define demo-scope (new-multi-scope))
(define demo-stx (add-scope empty-syntax demo-scope))

(syntax-context-require! demo-stx 0 (current-namespace) '#%core)
(syntax-context-require/expansion-time! demo-stx 1 (current-namespace) '#%core)

(define (compile+eval-expression e)
  (define c
    (compile (expand (datum->syntax demo-stx e))))
  (values c
          (run-time-eval c)))

(define (eval-expression e #:check [check-val #f])
  (define-values (c v) (compile+eval-expression e))
  (when check-val
    (unless (equal? v check-val)
      (error "check failed")))
  v)
  
(compile+eval-expression
 '(case-lambda
   [(x) (set! x 5)]
   [(x y) (begin0 y x)]
   [() (with-continuation-mark 1 2 3)]))

(compile+eval-expression
 '(lambda (x) (define-values (y) x) y))

(compile+eval-expression
 '(lambda (x)
   (define-syntaxes (y) (lambda (stx) (quote-syntax 7)))
   y))

(compile+eval-expression
 '(let-values ([(z) 9])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])
    ([(x) 5] [(y) (lambda (z) z)])
    (let-values ([(z) 10])
      (begin z (if (m 10) 1 2))))))

"expansion not captured"
(eval-expression
 #:check 'x-1
 '(let-values ([(x) 'x-1])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (quote-syntax x))])
    ()
    (let-values ([(x) 'x-3])
      (m)))))

"non-capturing expansion"
(eval-expression
 #:check 'x-3
 '(let-values ([(x) 'x-1])
   (letrec-syntaxes+values
    ([(m) (lambda (stx)
            (datum->syntax
             #f
             (list (quote-syntax let-values)
                   (list (list (list (quote-syntax x))
                               (quote-syntax 'x-2)))
                   (car (cdr (syntax-e stx))))))])
    ()
    (let-values ([(x) 'x-3])
      (m x)))))

"distinct generated variables"
(eval-expression
 #:check '(2 1)
 '(letrec-syntaxes+values
   ([(gen) (lambda (stx)
             (let-values ([(vals) (syntax-e (car (cdr (syntax-e stx))))]
                          [(binds) (syntax-e (car (cdr (cdr (syntax-e stx)))))]
                          [(refs) (syntax-e (car (cdr (cdr (cdr (syntax-e stx))))))])
               (datum->syntax
                #f
                (if (null? vals)
                    (list (quote-syntax bind) binds refs)
                    (list (quote-syntax gen)
                          (cdr vals)
                          (cons (list (list (quote-syntax x))
                                      (car vals))
                                binds)
                          (cons (quote-syntax x)
                                refs))))))]
    [(bind) (lambda (stx)
              (let-values ([(binds) (car (cdr (syntax-e stx)))]
                           [(refs) (car (cdr (cdr (syntax-e stx))))])
                (datum->syntax
                 (quote-syntax here)
                 (list (quote-syntax let-values)
                       binds
                       (cons (quote-syntax list)
                             refs)))))])
   ()
   (gen (1 2) () ())))

"use-site scopes (so not ambiguous)"
(eval-expression
 #:check 'ok
 '((let-values ()
     (define-syntaxes (identity)
       (lambda (stx)
         (let-values ([(misc-id) (car (cdr (syntax-e stx)))])
           (datum->syntax
            (quote-syntax here)
            (list 'lambda '(x)
                  (list 'let-values (list
                                     (list (list misc-id) ''other))
                        'x))))))
     (identity x))
   'ok))

"use-site scope remove from binding position"
(eval-expression
 #:check 'still-ok
 '(let-values ()
   (define-syntaxes (define-identity)
     (lambda (stx)
       (let-values ([(id) (car (cdr (syntax-e stx)))])
         (datum->syntax
          (quote-syntax here)
          (list 'define-values (list id) '(lambda (x) x))))))
   (define-identity f)
   (f 'still-ok)))

"compile-time scopes pruned by `quote-syntax`"
(syntax-context-require/expansion-time! demo-stx 2 (current-namespace) '#%core)
(eval-expression
 #:check 'bound
 '(letrec-syntaxes+values
   ([(m)
     (lambda (stx)
       (let-values ([(id1) (let-values ([(x) 1])
                             (define-syntaxes (wrap) ; to provoke a use-site scope
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (wrap (quote-syntax x)))]
                    [(id2) (let-values ([(x) 1])
                             (define-syntaxes (wrap)
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (wrap (quote-syntax x)))])
         (datum->syntax
          (quote-syntax here)
          (list 'let-values (list (list (list id1) ''bound))
                id2))))])
   ()
   (m)))

"`(quote-syntax .... #:local)` doesn't prune"
(eval-expression
 #:check 'bound-2
 '(letrec-syntaxes+values
   ([(m)
     (lambda (stx)
       (let-values ([(id1) (let-values ([(x) 1])
                             (quote-syntax x #:local))]
                    [(id2) (let-values ([(x) 1])
                             (define-syntaxes (wrap)
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (quote-syntax x #:local))])
         (datum->syntax
          (quote-syntax here)
          (list 'let-values (list (list (list id1) ''bound-1)
                                  (list (list id2) ''bound-2))
                id2))))])
   ()
   (m)))

;; ----------------------------------------

(define (eval-module-declaration mod)
  (run-time-eval
   (compile
    (expand (datum->syntax demo-stx mod) 
            (struct-copy expand-context (current-expand-context)
                         [context 'top-level]
                         [module-scopes (list demo-scope)])))))

(eval-module-declaration '(module m1 '#%core
                           (#%require (for-syntax '#%core))
                           (begin-for-syntax
                             (define-values (ten) (quote-syntax 10)))
                           (define-syntaxes (m) (lambda (stx) ten))
                           (define-values (x) 1)
                           (println x)
                           (#%provide (prefix-all-defined def:))
                           (println (m))
                           (m)))

(eval-module-declaration '(module m2 '#%core
                           (#%require 'm1)
                           (println def:x)))

"print 1 10 1"
(syntax-context-require! demo-stx 0 (current-namespace) 'm2)

;; ----------------------------------------

(eval-module-declaration '(module random-n '#%core
                           (define-values (n) (random))
                           (#%provide n)))

(eval-module-declaration '(module use-random-n '#%core
                           (#%require 'random-n
                                      (for-syntax '#%core
                                                  'random-n))
                           (define-syntaxes (m)
                             (lambda (stx) (datum->syntax (quote-syntax here)
                                                     n)))
                           (println (m))
                           (println (m))
                           (println n)
                           (println n)))

"print same number twive, then different number twice"
(syntax-context-require! demo-stx 0 (current-namespace) 'use-random-n)

;; ----------------------------------------

;; Fresh compile-time, same run-time:
(eval-module-declaration '(module use-random-n-again '#%core
                           (#%require 'random-n
                                      (for-syntax '#%core
                                                  'random-n))
                           (define-syntaxes (m)
                             (lambda (stx) (datum->syntax (quote-syntax here)
                                                     n)))
                           (println (m))
                           (println n)))

"first number is fresh, second number is same"
(syntax-context-require! demo-stx 0 (current-namespace) 'use-random-n-again)

;; ----------------------------------------

;; Check phase shifting of syntax objects:
(eval-module-declaration '(module two-xes '#%core
                           (#%require (for-syntax '#%core))
                           (define-values (x) 0)
                           (begin-for-syntax
                            (define-values (x) 1))
                           (#%provide x
                                      (for-syntax x))))

(eval-module-declaration '(module use-two-xes '#%core
                           (#%require (for-template 'two-xes)
                                      (for-syntax '#%core))
                           (define-values (rt-x-ref) (quote-syntax x))
                           (begin-for-syntax
                             (define-values (ct-x-ref) (quote-syntax x)))
                           (#%provide rt-x-ref
                                      (for-syntax ct-x-ref))))

(eval-module-declaration '(module use-x-ref '#%core
                           (#%require 'use-two-xes
                                      (for-syntax '#%core
                                                  'use-two-xes))
                           (define-syntaxes (ct-m) (lambda (stx) ct-x-ref))
                           (define-syntaxes (rt-m) (lambda (stx) rt-x-ref))
                           (println (ct-m))
                           (println (rt-m))))

"print 1 then 0"
(syntax-context-require! demo-stx 0 (current-namespace) 'use-x-ref)
