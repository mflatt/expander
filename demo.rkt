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
(syntax-context-require! demo-stx 1 (current-namespace) '#%core)

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


(run-time-eval
 (compile
  (expand (datum->syntax demo-stx '(module m1 '#%core
                                    (#%require (for-syntax '#%core))
                                    (define-syntaxes (m) (lambda (stx) (quote-syntax 10)))
                                    (define-values (x) 1)
                                    (println x)
                                    (#%provide (prefix-all-defined def:))
                                    (m)))
          (struct-copy expand-context (current-expand-context)
                       [context 'top-level]
                       [current-module-scopes (list demo-scope)]))))

(run-time-eval
 (compile
  (expand (datum->syntax demo-stx '(module m2 '#%core
                                    (#%require 'm1)
                                    (println def:x)))
          (struct-copy expand-context (current-expand-context)
                       [context 'top-level]
                       [current-module-scopes (list demo-scope)]))))

(syntax-context-require! demo-stx 0 (current-namespace) 'm1)
(syntax-context-require! demo-stx 0 (current-namespace) 'm2)
