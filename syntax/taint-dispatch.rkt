#lang racket/base
(require "syntax.rkt"
         "property.rkt"
         "to-list.rkt"
         "scope.rkt"
         "../namespace/core.rkt"
         "original.rkt")

;; The `taint-dispatch` function recognizes syntax properties and
;; bindings that adjust the way that a syntax object is armed.

(provide taint-dispatch
         syntax-remove-taint-dispatch-properties)

(define (taint-dispatch s proc phase)
  (let loop ([s s] [mode (syntax-taint-mode-property s)])
    (case mode
      [(opaque) (proc s)]
      [(transparent)
       (define c (syntax-map (or (syntax->list s)
                                 (syntax-e s))
                             (lambda (tail? d) d)
                             (lambda (s d) (loop s (syntax-taint-mode-property s)))
                             #f))
       (datum->syntax #f c s (if (syntax-any-macro-scopes? s)
                                 (syntax-property-remove s original-property-sym)
                                 s))]
      [(transparent-binding)
       (define c (syntax-e s))
       (cond
        [(pair? c)
         (define cd (if (syntax? (cdr c))
                        (syntax-e (cdr c))
                        (cdr c)))
         (cond
          [(or (pair? cd)
               (and (syntax? cd) (pair? (syntax-e cd))))
           (define d (if (syntax? cd) (syntax-e cd) cd))
           (datum->syntax s
                          (cons (loop (car c) (syntax-taint-mode-property s))
                                (cons (loop (car d) 'transparent)
                                      (syntax-map (syntax->list (cdr d))
                                                  (lambda (tail? d) d)
                                                  (lambda (s d) (loop s (syntax-taint-mode-property s)))
                                                  #f)))
                          s
                          s)]
          [else (loop s 'transparent)])]
        [else (loop s 'transparent)])]
      [else
       (define c (syntax-e s))
       (case (core-form-sym c phase)
         [(begin begin-for-syntax module #%module-begin)
          (loop s 'transparent)]
         [(define-values define-syntaxes)
          (loop s 'transparent-binding)]
         [else
          (loop s 'opaque)])])))

;; ----------------------------------------

(define (syntax-taint-mode-property s)
  (or (syntax-property s 'taint-mode)
      (syntax-property s 'certify-mode)))

(define (syntax-remove-taint-dispatch-properties s)
  (syntax-property-remove (syntax-property-remove s 'taint-mode) 'use-mode))
