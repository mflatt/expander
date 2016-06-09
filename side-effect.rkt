#lang racket/base
(require "set.rkt"
         "built-in-symbol.rkt"
         "correlate.rkt")

;; To support extraction of a bootstrapped version of the expander, we
;; need to be able to prune unused module content. Pruning is usefully
;; improved by a simple analysis of whether a module body has any
;; side-effects.

(provide any-side-effects?)

(define (any-side-effects? e ; compiled expression
                           expected-results) ; number of expected reuslts, or #f if any number is ok
  (define actual-results
    (let loop ([e e])
      (case (and (pair? (correlated-e e))
                 (correlated-e (car (correlated-e e))))
        [(quote lambda case-lambda) 1]
        [(letrec-values)
         (define m (match-correlated e '(_ ([ids rhs] ...) body)))
         (and (not (for/or ([ids (in-list (m 'ids))]
                            [rhs (in-list (m 'rhs))])
                     (any-side-effects? rhs (correlated-length ids))))
              (loop (m 'body)))]
        [(make-struct-type)
         (and (ok-make-struct-type? e)
              5)]
        [else
         (define v (correlated-e e))
         (and (symbol? v)
              (or (built-in-symbol? v)
                  ;; FIXME: needed for "kernstruct.rkt"
                  (eq? v 'exn:fail:syntax))
              1)])))
  (not (and actual-results
            (or (not expected-results)
                (= actual-results expected-results)))))

;; ----------------------------------------

(define (ok-make-struct-type? e)
  (define l (correlated->list e))
  (define init-field-count-expr (and ((length l) . > . 3)
                                     (list-ref l 3)))
  (define immutables-expr (or (and ((length l) . > . 9)
                                   (list-ref l 9))
                              'null))  
  (and ((length l) . >= . 5)
       ((length l) . <= . 12)
       (for/and ([arg (cdr l)]
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
  (and (pair? (correlated-e v))
       (eq? (correlated-e (car (correlated-e v))) 'quote)
       (val? (correlated-e (correlated-cadr v)))))

(define (false? v)
  (eq? (correlated-e v) #f))

(define (field-count-expr-to-field-count v)
  (and (quoted? exact-nonnegative-integer? v)
       (correlated-e (correlated-cadr v))))

(define (inspector-or-false? v)
  (or (quoted? false? v)
      (and (= 1 (correlated-length v))
           (eq? 'current-inspector (correlated-e (car (correlated-e v)))))))

(define (known-good-struct-properties? v immutables-expr)
  (or (quoted? null? v)
      (and (pair? (correlated-e v))
           (eq? (correlated-e (car (correlated-e v))) 'list)
           (for/and ([prop+val (in-list (cdr (correlated->list v)))])
             (and (= (correlated-length prop+val) 3)
                  (let ([prop+val (correlated->list prop+val)])
                    (and (eq? 'cons (correlated-e (car prop+val)))
                         (known-good-struct-property+value? (list-ref prop+val 1)
                                                            (list-ref prop+val 2)
                                                            immutables-expr)))))
           ;; All properties must be distinct
           (= (sub1 (correlated-length v))
              (set-count (for/set ([prop+val (in-list (cdr  (correlated->list v)))])
                           (correlated-e (list-ref (correlated->list prop+val) 1))))))))

(define (known-good-struct-property+value? prop-expr val-expr immutables-expr)
  (case (correlated-e prop-expr)
    [(prop:evt) (immutable-field? val-expr immutables-expr)]
    [(prop:procedure) (immutable-field? val-expr immutables-expr)]
    [else #f]))

(define (immutable-field? val-expr immutables-expr)
  (and (quoted? exact-nonnegative-integer? val-expr)
       (memv (correlated-e (correlated-cadr val-expr))
             (immutables-expr-to-immutables immutables-expr null))))

(define (immutables-expr-to-immutables e fail-v)
  (case (and (pair? (correlated-e e))
             (correlated-e (car (correlated-e e))))
    [(quote)
     (define v (correlated-cadr e))
     (or (and (correlated-length v)
              (let ([l (map correlated-e (correlated->list v))])
                (and (andmap exact-nonnegative-integer? l)
                     (= (length l) (set-count (list->set l)))
                     l)))
         fail-v)]
    [else fail-v]))

(define (procedure-spec? e immutables-expr)
  (or (quoted? false? e)
      (and (quoted? exact-nonnegative-integer? e)
           (memv (correlated-e (correlated-cadr e))
                 (immutables-expr-to-immutables immutables-expr null)))))

(define (immutables-ok? e init-field-count-expr)
  (define l (immutables-expr-to-immutables e #f))
  (define c (field-count-expr-to-field-count init-field-count-expr))
  (and l
       (for/and ([n (in-list l)])
         (n . < . c))))

;; ----------------------------------------

(module+ test
  (define-syntax-rule (check expr result)
    (unless (equal? expr result)
      (error 'failed "~s" #'expr)))
  
  (define (any-side-effects?* e n)
    (define v1 (any-side-effects? e n))
    (define v2 (any-side-effects? (datum->correlated e) n))
    (unless (equal? v1 v2)
      (error "problem with correlated:" e))
    v1)
  
  (check (any-side-effects?* ''1 1)
         #f)

  (check (any-side-effects?* ''1 #f)
         #f)

  (check (any-side-effects?* '(lambda (x) x) 1)
         #f)
  
  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list (cons prop:evt '0))
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               '()
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               '()
                               (current-inspector)
                               '0
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list
                                (cons prop:evt '0)
                                (cons prop:evt '0)) ; duplicate
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #t)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list (cons prop:evt '0))
                               (current-inspector)
                               '#f
                               '(1)) ; <- too big
                             5)
         #t))


