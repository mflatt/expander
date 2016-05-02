#lang racket

(provide
 (struct-out syntax)
 empty-syntax
 
 syntax->datum
 datum->syntax
 
 identifier?
 
 syntax-property)

(struct syntax (e      ; datum and nested syntax objects
                scopes ; scopes that apply at all phases
                shifted-multi-scopes ; scopes with a distinct identity at each phase
                srcloc ; source location
                props) ; properties
        ;; Custom printer:
        #:property prop:custom-write
        (lambda (s port mode)
          (write-string "#<syntax:" port)
          (fprintf port "~.s" (syntax->datum s))
          (write-string ">" port)))

(define empty-scopes (seteq))
(define empty-shifted-multi-scopes (set))
(define empty-props #hash())

(define empty-syntax
  (syntax #f empty-scopes empty-shifted-multi-scopes #f empty-props))

(define (syntax->datum s)
  (let loop ([s (syntax-e s)])
    (cond
     [(syntax? s) (loop (syntax-e s))]
     [(pair? s) (cons (loop (car s))
                      (loop (cdr s)))]
     [else s])))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f])
  (define (wrap e)
    (syntax e
            (if stx-c
                (syntax-scopes stx-c)
                empty-scopes)
            (if stx-c
                (syntax-shifted-multi-scopes stx-c)
                empty-shifted-multi-scopes)
            (and stx-l (syntax-srcloc stx-l))
            (if stx-p (syntax-props stx-p) empty-props)))
  (cond
   [(syntax? s) s]
   [(list? s) (wrap (for/list ([e (in-list s)])
                      (datum->syntax stx-c e stx-l stx-p)))]
   [(pair? s) (wrap (cons (datum->syntax stx-c (car s) stx-l stx-p)
                          (datum->syntax stx-c (cdr s) stx-l stx-p)))]
   [else (wrap s)]))

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-e s))))

(define syntax-property
  (case-lambda
    [(s key)
     (unless (syntax? s)
       (raise-argument-error 'syntax-property "syntax" s))
     (hash-ref (syntax-props s) key #f)]
    [(s key val)
     (unless (syntax? s)
       (raise-argument-error 'syntax-property "syntax" s))
     (struct-copy syntax s
                  [props (hash-set (syntax-props s) key val)])]))
