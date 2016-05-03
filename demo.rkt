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

(define (eval-module-declaration mod)
  (run-time-eval
   (compile
    (expand (datum->syntax demo-stx mod) 
            (struct-copy expand-context (current-expand-context)
                         [context 'top-level]
                         [current-module-scopes (list demo-scope)])))))

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
