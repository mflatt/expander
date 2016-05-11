#lang racket/base

(provide make-match-syntax)

(define (make-match-syntax syntax? identifier? syntax-e)
  ;; A lightweight pattern matcher along the lines of `syntax-rules`.
  ;; The result of matching is a function that takes a symbol and
  ;; returns its match.
  (define (match-syntax orig-s pattern
                        #:error [error error])
    (define (match s pattern)
      (cond
       [(symbol? pattern)
        (when (regexp-match? #rx"^id(:|$)" (symbol->string pattern))
          (unless (identifier? s)
            (error "not an identifier:" s)))
        (list (list pattern s))]
       [(syntax? s) (match (syntax-e s) pattern)]
       [(and (list? pattern)
             (= (length pattern) 2)
             (or (eq? '... (cadr pattern))
                 (eq? '...+ (cadr pattern))))
        (define flat-s (to-syntax-list s))
        (cond
         [(null? flat-s)
          (when (eq? '...+ (cadr pattern))
            (error "bad syntax:" orig-s))
          (make-empty-vars pattern)]
         [(list? flat-s)
          (define a-lists
            (for/list ([s (in-list flat-s)])
              (match s (car pattern))))
          (apply map
                 (lambda slice
                   (list (caar slice)
                         (map cadr slice)))
                 a-lists)]
         [else (error "bad syntax:" orig-s)])]
       [(pair? pattern)
        (cond
         [(pair? s)
          (append (match (car s) (car pattern))
                  (match (cdr s) (cdr pattern)))]
         [else (error "bad syntax:" orig-s)])]
       [(null? pattern)
        (cond
         [(null? s) null]
         [else (error "bad syntax:" orig-s)])]
       [(and (or (keyword? pattern)
                 (boolean? pattern))
             (eq? pattern s))
        null]
       [else
        (error "bad pattern")]))
    (define a-list (match orig-s pattern))
    (lambda (sym)
      (define a (assq sym a-list))
      (if a
          (cadr a)
          (error "no such pattern variable:" sym))))

  (define (make-empty-vars pattern)
    (cond
     [(symbol? pattern)
      (list (list pattern null))]
     [(and (list? pattern)
           (= (length pattern) 2)
           (or (eq? '... (cadr pattern))
               (eq? '...+ (cadr pattern))))
      (map (lambda (m)
             (cons (car m) (list (cadr m))))
           (make-empty-vars (car pattern)))]
     [(pair? pattern)
      (append (make-empty-vars(car pattern))
              (make-empty-vars(cdr pattern)))]
     [else
      null]))

  (define (to-syntax-list s)
    (cond
     [(pair? s) (cons (car s) (to-syntax-list (cdr s)))]
     [(syntax? s) (to-syntax-list (syntax-e s))]
     [else s]))
  
  match-syntax)
