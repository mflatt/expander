#lang racket/base

(provide register-built-in-symbol!
         built-in-symbol?
         make-built-in-symbol!)

(define built-in-symbols (make-hasheq))

(define (register-built-in-symbol! s)
  (hash-set! built-in-symbols s #t))

(define (built-in-symbol? s)
  (hash-ref built-in-symbols s #f))

(define (make-built-in-symbol! s)
  ;; Make a symbol that is a little more obscure than just `s`
  (define built-in-s (string->symbol (format ".~s" s)))
  (register-built-in-symbol! built-in-s)
  built-in-s)

;; ----------------------------------------

;; Primitive expression forms
(for-each register-built-in-symbol!
          '(lambda case-lambda
            if begin begin0
            let-values letrec-values
            set! quote
            with-continuation-mark
            #%variable-reference))
