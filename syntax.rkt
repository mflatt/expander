#lang racket

(provide
 (struct-out syntax) ; includes `syntax?` and `syntax-e`
 empty-syntax
 identifier?
 
 syntax->datum
 datum->syntax)

(struct syntax (e        ; datum and nested syntax objects
                scopes)  ; scopes that apply at all phases
        #:transparent)

(define empty-scopes (seteq))

(define empty-syntax
  (syntax #f empty-scopes))

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-e s))))

(define (syntax->datum s)
  (let loop ([s (syntax-e s)])
    (cond
     [(syntax? s) (loop (syntax-e s))]
     [(pair? s) (cons (loop (car s))
                      (loop (cdr s)))]
     [else s])))

(define (datum->syntax stx-c s)
  (define (wrap e)
    (syntax e (if stx-c
                  (syntax-scopes stx-c)
                  empty-scopes)))
  (cond
   [(syntax? s) s]
   [(list? s) (wrap (for/list ([e (in-list s)])
                      (datum->syntax stx-c e)))]
   [(pair? s) (wrap (cons (datum->syntax stx-c (car s))
                          (datum->syntax stx-c (cdr s))))]
   [else (wrap s)]))
