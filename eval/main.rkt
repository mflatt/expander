#lang racket/base
(require (prefix-in direct: "eval.rkt")
         (prefix-in direct: "../namespace/eval.rkt")
         "../syntax/checked-syntax.rkt"
         "../namespace/namespace.rkt"
         "../common/contract.rkt")

;; These wrappers implement the protocol for whether to use
;; `namespace-synatx-introduce` on the argument to `eval`, etc.

(provide eval
         eval-syntax
         
         compile
         compile-syntax
         
         expand
         expand-syntax
         
         expand-to-top-form
         expand-syntax-to-top-form)

(define (eval s [ns (current-namespace)])
  (check 'eval namespace? ns)
  (parameterize ([current-namespace ns])
    ((current-eval) (intro s ns))))

(define (eval-syntax s [ns (current-namespace)])
  (check 'eval syntax? s)
  (check 'eval namespace? ns)
  (parameterize ([current-namespace ns])
    ((current-eval) s)))

(define (compile s)
  ((current-compile) (intro s) #f))

(define (compile-syntax s)
  (check 'compile syntax? s)
  ((current-compile) (intro s)))

(define (expand s)
  (direct:expand (intro s)))

(define (expand-syntax s)
  (check 'expand syntax? s)
  (direct:expand s))

(define (expand-to-top-form s)
  (direct:expand-to-top-form (intro s)))

(define (expand-syntax-to-top-form s)
  (check 'expand syntax? s)
  (direct:expand-to-top-form s))


(define (intro given-s [ns (current-namespace)])
  (define s (if (syntax? given-s) given-s (datum->syntax #f given-s)))
  (direct:namespace-syntax-introduce s ns))
