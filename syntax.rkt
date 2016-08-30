#lang racket

(provide
 (struct-out syntax) ; includes `syntax?` and `syntax-e`
 empty-syntax
 identifier?
 bound-identifier=?
 
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

(define (bound-identifier=? a b)
  (and (eq? (syntax-e a) (syntax-e b))
       (equal? (syntax-scopes a) (syntax-scopes b))))

(define (syntax->datum s)
  (let ([e (syntax-e s)])
    (cond
     [(list? e) (map syntax->datum e)]
     [else e])))

(define (datum->syntax stx-c v)
  (define (wrap e)
    (syntax e (if stx-c
                  (syntax-scopes stx-c)
                  empty-scopes)))
  (cond
   [(syntax? v) v]
   [(list? v) (wrap (map (lambda (elem-v) (datum->syntax stx-c elem-v))
                         v))]
   [else (wrap v)]))
