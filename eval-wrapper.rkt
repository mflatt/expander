#lang racket/base
(require (prefix-in direct: "eval.rkt")
         (prefix-in direct: "namespace-eval.rkt")
         "checked-syntax.rkt"
         "namespace.rkt"
         "contract.rkt")

(provide eval
         eval-syntax
         
         compile
         compile-syntax
         
         expand
         expand-syntax)

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


(define (intro given-s [ns (current-namespace)])
  (define s (if (syntax? given-s) given-s (datum->syntax #f given-s)))
  (direct:namespace-syntax-introduce s ns))
