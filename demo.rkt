#lang racket/base
(require "main.rkt")

;; ----------------------------------------

(define (expand-expression e)
  (expand (introduce (datum->syntax e))))

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
                               (map car (car (cdr stx)))
                               (car (cdr (cdr stx))))
                         (map (lambda (b)
                                (car (cdr b)))
                              (car (cdr stx))))))])
    ,e))

(compile+eval-expression
 (add-let
  '(lambda (x) 
    (let ([y x])
      y))))

(compile+eval-expression
 '(lambda (x)
   (let-syntax ([y (lambda (stx) (quote-syntax '7))])
     (y))))

(compile+eval-expression
 (add-let
  '(let ([z '9])
    (let-syntax ([m (lambda (stx) (car (cdr stx)))])
      (let ([x '5])
        (let ([y (lambda (z) z)])
          (let ([z '10])
            (list z (m '10)))))))))

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
                             (car (cdr stx)))))])
      (let ([x 'x-3])
        (m x))))))

"distinct generated variables via introduction scope"
;; Essentially the same as
;;   (define-syntax-rule (gen2 _ x1 x2 v1 v2)
;;     (let ([x1 v1])
;;       (let ([v2 v2])
;;         (list x1 x2)))))
;;   (define-syntax-rule (gen1 next . rest)
;;     (next gen2 x . rest)) ; <- `x` twice in final expansion
;;   (gen1 gen1 1 2)
;; to check that the two introduced instances of `x` are
;; not `bound-identifier=?`
(eval-expression
 #:check '(1 2)
 (add-let
  `(let-syntax ([gen2 (lambda (stx)
                        (datum->syntax
                         (list (quote-syntax let)
                               (list (list (car (cdr (cdr stx)))
                                           (car (cdr (cdr (cdr (cdr stx)))))))
                               (list (quote-syntax let)
                                     (list (list (car (cdr (cdr (cdr stx))))
                                                 (car (cdr (cdr (cdr (cdr (cdr stx))))))))
                                     (list (quote-syntax list)
                                           (car (cdr (cdr stx)))
                                           (car (cdr (cdr (cdr stx)))))))))])
    (let-syntax ([gen1 (lambda (stx)
                         (datum->syntax
                          (cons (car (cdr stx))
                                (cons (quote-syntax gen2)
                                      (cons (quote-syntax x)
                                            (cdr (cdr stx)))))))])
      (gen1 gen1 '1 '2)))))
