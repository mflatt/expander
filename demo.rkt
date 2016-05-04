#lang racket/base
(require "main.rkt")

;; ----------------------------------------

(define (expand-expression e)
  (expand (namespace-syntax-introduce (datum->syntax #f e))))

(define (compile+eval-expression e)
  (define c
    (compile (expand-expression e)))
  (values c
          (eval c)))

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

"non-transformer binding misuse"
(with-handlers ([exn:fail? (lambda (exn)
                             (unless (regexp-match? #rx"illegal use of syntax"
                                                    (exn-message exn))
                               (error "wrong error"))
                             'illegal-use)])
  (expand-expression '(letrec-syntaxes+values
                       ([(v) 1])
                       ()
                       v))
  (error "shouldn't get here"))
