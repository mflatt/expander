#lang racket/base
(require (only-in "reader-syntax.rkt"
                  [make-readtable host:make-readtable])
         "../syntax/syntax.rkt"
         "syntax-to-reader-syntax.rkt")

(provide make-readtable)

;; Adjust readtable results for host's reader syntax

(define (make-readtable rt . args)
  ;; For error checking:
  (apply host:make-readtable rt args)
  ;; For wrapping:
  (apply host:make-readtable
         rt
         (let loop ([args args])
           (cond
            [(null? args) args]
            [else
             (list* (car args)
                    (cadr args)
                    (let ([p (caddr args)])
                      (if (procedure? p)
                          (wrap-procedure p)
                          p))
                    (loop (cdddr args)))]))))

(define (wrap-procedure p)
  (cond
   [(and (procedure-arity-includes? p 2)
         (procedure-arity-includes? p 6))
    (case-lambda
      [(ch in)
       (define v (p ch in))
       (if (syntax? v)
           ;; 2 arguments => in `read` mode
           (syntax->datum v)
           v)]
      [(ch in line col pos span)
       (define v (p ch in line col pos span))
       (if (syntax? v)
           (syntax->reader-syntax v)
           v)])]
   [(procedure-arity-includes? p 2)
    (lambda (ch in)
      (define v (p ch in))
      (if (syntax? v)
          (syntax->reader-syntax v)
          v))]
   [(procedure-arity-includes? p 6)
    (lambda (ch in line col pos span)
      (define v (p ch in line col pos span))
      (if (syntax? v)
          (syntax->reader-syntax v)
          v))]))
