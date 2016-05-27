#lang racket/base
(require (prefix-in direct: "eval.rkt")
         (only-in "syntax.rkt" syntax?)
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
  (check 'eval syntax? ns)
  (check 'eval namespace? ns)
  (parameterize ([current-namespace ns])
    ((current-eval) s)))

(define (compile s)
  ((current-compile) (intro s) #f))

(define (compile-syntax s)
  (check 'compile syntax? s)
  ((current-compile) (intro s) #f))

(define (expand s)
  (direct:expand (intro s) #f))

(define (expand-syntax s)
  (check 'expand syntax? s)
  (direct:expand s))


(define (intro s [ns (current-namespace)])
  (if (syntax? s)
      s
      (direct:namespace-syntax-introduce (datum->syntax #f s) ns)))
