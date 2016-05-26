#lang racket/base
(require "set.rkt")

;; To support extraction of a bootstrapped version of the expander, we
;; need to be able to prune unused module content. Pruning is usefully
;; improved by a simple analysis of whether a module body has any
;; side-effects.

(provide any-side-effects?)

(define (any-side-effects? e ; compiled expression
                           expected-results) ; number of expected reuslts, or #f if any number is ok
  (define actual-results
    (let loop ([e e])
      (case (and (pair? e) (car e))
        [(quote lambda case-lambda) 1]
        [(letrec-values)
         (and (not (for/or ([binding (in-list (cadr e))])
                     (any-side-effects? (cadr binding) (length (car binding)))))
              (loop (caddr e)))]
        [(make-struct-type)
         (and (ok-make-struct-type? e)
              5)]
        [else #f])))
  (not (and actual-results
            (or (not expected-results)
                (= actual-results expected-results)))))

;; ----------------------------------------

(define (ok-make-struct-type? e)
  (define init-field-count-expr (and ((length e) . > . 3)
                                     (list-ref e 3)))
  (define immutables-expr (or (and ((length e) . > . 9)
                                   (list-ref e 9))
                              'null))  
  (and ((length e) . >= . 5)
       ((length e) . <= . 12)
       (for/and ([arg (cdr e)]
                 [pred (list
                        (lambda (v) (quoted? symbol? v))
                        (lambda (v) (quoted? false? v))
                        (lambda (v) (field-count-expr-to-field-count v))
                        (lambda (v) (field-count-expr-to-field-count v))
                        (lambda (v) (not (any-side-effects? v 1)))
                        (lambda (v) (known-good-struct-properties? v immutables-expr))
                        (lambda (v) (inspector-or-false? v))
                        (lambda (v) (procedure-spec? v immutables-expr))
                        (lambda (v) (immutables-ok? v init-field-count-expr)))])
         (pred arg))))

(define (quoted? val? v)
  (and (pair? v)
       (eq? (car v) 'quote)
       (val? (cadr v))))

(define (false? v)
  (eq? v #f))

(define (field-count-expr-to-field-count v)
  (and (quoted? exact-nonnegative-integer? v)
       (cadr v)))

(define (inspector-or-false? v)
  (or (quoted? false? v)
      (and (pair? v)
           (= 1 (length v))
           (eq? 'current-inspector (car v)))))

(define (known-good-struct-properties? v immutables-expr)
  (or (quoted? null? v)
      (and (pair? v)
           (eq? (car v) 'list)
           (for/and ([prop+val (in-list (cdr v))])
             (and (pair? prop+val)
                  (eq? 'cons (car prop+val))
                  (= (length prop+val) 3)
                  (known-good-struct-property+value? (list-ref prop+val 1)
                                                     (list-ref prop+val 2)
                                                     immutables-expr)))
           ;; All properties must be distinct
           (= (length (cdr v))
              (set-count (for/set ([prop+val (in-list (cdr v))])
                           (list-ref prop+val 1)))))))

(define (known-good-struct-property+value? prop-expr val-expr immutables-expr)
  (case prop-expr
    [(prop:evt) (immutable-field? val-expr immutables-expr)]
    [(prop:procedure) (immutable-field? val-expr immutables-expr)]
    [else #f]))

(define (immutable-field? val-expr immutables-expr)
  (and (quoted? exact-nonnegative-integer? val-expr)
       (memv (cadr val-expr) (immutables-expr-to-immutables immutables-expr null))))

(define (immutables-expr-to-immutables e fail-v)
  (case (and (pair? e) (car e))
    [(quote)
     (or (and (list? (cadr e))
              (let ([l (cadr e)])
                (and (list? l)
                     (andmap exact-nonnegative-integer? l)
                     (= (length l) (set-count (list->set l)))
                     l)))
         fail-v)]
    [else fail-v]))

(define (procedure-spec? e immutables-expr)
  (or (quoted? false? e)
      (and (quoted? exact-nonnegative-integer? e)
           (memv (cadr e) (immutables-expr-to-immutables immutables-expr null)))))

(define (immutables-ok? e init-field-count-expr)
  (define l (immutables-expr-to-immutables e #f))
  (define c (field-count-expr-to-field-count init-field-count-expr))
  (and l
       (for/and ([n (in-list l)])
         (n . < . c))))

(module+ test
  (define-syntax-rule (check expr result)
    (unless (equal? expr result)
      (error 'failed "~s" #'expr)))
  
  (check (any-side-effects? ''1 1)
         #f)

  (check (any-side-effects? ''1 #f)
         #f)

  (check (any-side-effects? '(lambda (x) x) 1)
         #f)
  
  (check (any-side-effects? '(make-struct-type 'evt '#f '1 '0 '#f
                              (list (cons prop:evt '0))
                              (current-inspector)
                              '#f
                              '(0))
                            5)
         #f)

  (check (any-side-effects? '(make-struct-type 'evt '#f '1 '0 '#f
                              '()
                              (current-inspector)
                              '#f
                              '(0))
                            5)
         #f)

  (check (any-side-effects? '(make-struct-type 'evt '#f '1 '0 '#f
                              '()
                              (current-inspector)
                              '0
                              '(0))
                            5)
         #f)

  (check (any-side-effects? '(make-struct-type 'evt '#f '1 '0 '#f
                              (list
                                     (cons prop:evt '0)
                                     (cons prop:evt '0)) ; duplicate
                              (current-inspector)
                              '#f
                              '(0))
                            5)
         #t)

  (check (any-side-effects? '(make-struct-type 'evt '#f '1 '0 '#f
                              (list (cons prop:evt '0))
                              (current-inspector)
                              '#f
                              '(1)) ; <- too big
                            5)
         #t))


