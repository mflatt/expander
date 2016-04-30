#lang racket

(provide
 (struct-out stx)
 empty-stx
 syntax?
 syntax-e
 syntax->datum
 datum->syntax
 identifier?)

(struct stx (e scopes shifted-multi-scopes srcloc props)
        #:property prop:custom-write
        (lambda (s port mode)
          (write-string "#<syntax:" port)
          (fprintf port "~.s" (syntax->datum s))
          (write-string ">" port)))

(define empty-scopes (seteq))
(define empty-shifted-multi-scopes (set))
(define empty-props #hash())

(define empty-stx
  (stx #f empty-scopes empty-shifted-multi-scopes #f empty-props))

(define (syntax? s)
  (stx? s))

(define (syntax-e s)
  (stx-e s))

(define (syntax->datum s)
  (let loop ([s (stx-e s)])
    (cond
     [(stx? s) (loop (stx-e s))]
     [(pair? s) (cons (loop (car s))
                      (loop (cdr s)))]
     [else s])))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f])
  (define (wrap e)
    (stx e
         (if stx-c
             (stx-scopes stx-c)
             empty-scopes)
         (if stx-c
             (stx-shifted-multi-scopes stx-c)
             empty-shifted-multi-scopes)
         (and stx-l (stx-srcloc stx-l))
         (if stx-p (stx-props stx-p) empty-props)))
  (cond
   [(stx? s) s]
   [(list? s) (wrap (for/list ([e (in-list s)])
                      (datum->syntax stx-c e stx-l stx-p)))]
   [(pair? s) (wrap (cons (datum->syntax stx-c (car s) stx-l stx-p)
                          (datum->syntax stx-c (cdr s) stx-l stx-p)))]
   [else (wrap s)]))

(define (identifier? s)
  (and (stx? s) (symbol? (stx-e s))))
