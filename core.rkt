#lang racket/base
(require racket/set
         racket/unit
         "syntax.rkt"
         "scope.rkt"
         "binding.rkt"
         "match.rkt")

(provide core-scope
         core-stx
         
         add-core-form!
         add-core-primitive!
         
         core-forms
         core-primitives
         
         core-form-sym)

;; Accumulate all core bindings in `core-scope`, so we can
;; easily generate a reference to a core form using `core-stx`:
(define core-scope (new-scope))
(define core-stx (add-scope empty-syntax core-scope))

;; Core forms are added by `require`s in "main.rkt"

;; Accumulate core forms and primitives:
(define core-forms (make-hasheq))
(define core-primitives (make-hasheq))

(define (add-core-form! sym proc)
  (add-core-binding! sym)
  (hash-set! core-forms sym proc))

(define (add-core-primitive! sym val)
  (add-core-binding! sym)
  (hash-set! core-primitives sym val))

(define (add-core-binding! sym)
  (add-binding! (datum->syntax core-stx sym)
                (core-binding sym)))

;; Helper for recognizing and dispatching on core forms:
(define (core-form-sym s)
  (define m (try-match-syntax s '(id . _)))
  (and m
       (let ([b (resolve (m 'id))])
         (and (core-binding? b)
              (core-binding-sym b)))))
