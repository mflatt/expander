#lang racket/base
(require "syntax.rkt")

(provide match-syntax
         try-match-syntax)

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
       [(null? flat-s) null]
       [(list? flat-s)
        (define a-lists
          (for/list ([s (in-list flat-s)])
            (match s (car pattern))))
        (when (and (eq? '...+ (cadr pattern))
                   (null? (car a-lists)))
          (error "bad syntax:" orig-s))
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
        ;; assume a sequence with 0 matches
        null)))

(define (try-match-syntax orig-s pattern)
  (let/ec esc
    (match-syntax orig-s pattern
                  #:error (lambda args (esc #f)))))

(define (to-syntax-list s)
  (cond
   [(pair? s) (cons (car s) (to-syntax-list (cdr s)))]
   [(syntax? s) (to-syntax-list (syntax-e s))]
   [else s]))
