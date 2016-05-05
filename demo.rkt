#lang racket/base
(require "main.rkt")

;; ----------------------------------------

(define (expand-expression e)
  (expand (namespace-syntax-introduce (datum->syntax e))))

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
 '(lambda (x) x))

;; examples are easier with `let`:
(define (add-let e)
  `(let-syntax ([let (lambda (stx)
                       (datum->syntax
                        (cons
                         (list (quote-syntax lambda)
                               (map (lambda (b)
                                      (car (syntax-e b)))
                                    (syntax-e (car (cdr (syntax-e stx)))))
                               (car (cdr (cdr (syntax-e stx)))))
                         (map (lambda (b)
                                (car (cdr (syntax-e b))))
                              (syntax-e (car (cdr (syntax-e stx))))))))])
    ,e))
(compile+eval-expression
 (add-let
  '(lambda (x) 
    (let ([y x])
      y))))

(compile+eval-expression
 '(lambda (x)
   (let-syntax ([y (lambda (stx) (quote-syntax 7))])
     y)))

(compile+eval-expression
 (add-let
  '(let ([z 9])
    (let-syntax ([m (lambda (stx) (car (cdr (syntax-e stx))))])
      (let ([x 5]
            [y (lambda (z) z)])
        (let ([z 10])
          (list z (m 10))))))))

"expansion not captured"
(eval-expression
 #:check 'x-1
 (add-let
  '(let ([x 'x-1])
    (let-syntax ([m (lambda (stx) (quote-syntax x))])
      (let ([x 'x-3])
        (m))))))

"non-capturing expansion"
(eval-expression
 #:check 'x-3
 (add-let
  '(let ([x 'x-1])
    (let-syntax ([m (lambda (stx)
                      (datum->syntax
                       (list (quote-syntax let)
                             (list (list (quote-syntax x)
                                         (quote-syntax 'x-2)))
                             (car (cdr (syntax-e stx))))))])
      (let ([x 'x-3])
        (m x))))))

"distinct generated variables"
#;
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


"non-transformer binding misuse"
(with-handlers ([exn:fail? (lambda (exn)
                             (unless (regexp-match? #rx"illegal use of syntax"
                                                    (exn-message exn))
                               (error "wrong error"))
                             'illegal-use)])
  (expand (namespace-syntax-introduce
           (datum->syntax '(let-syntax ([v 1])
                            v))))
  (error "shouldn't get here"))

