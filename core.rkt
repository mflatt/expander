#lang racket/base
(require racket/set
         racket/unit
         "syntax.rkt"
         "scope.rkt"
         "binding.rkt"
         "match.rkt"
         "namespace.rkt")

(provide core-scope
         core-stx
         
         add-core-form!
         add-core-primitive!
         
         declare-core-top-level!
         
         core-form-sym)

;; Accumulate all core bindings in `core-scope`, so we can
;; easily generate a reference to a core form using `core-stx`:
(define core-scope (new-scope))
(define core-stx (add-scope empty-syntax core-scope))

;; Core forms are added by `require`s in "main.rkt"

;; Accumulate added core forms and primitives:
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
                (top-level-binding sym)))

;; Used only after filling in all core forms and primitives:
(define (declare-core-top-level! ns)
  (for ([(sym val) (in-hash core-primitives)])
    (namespace-set-variable! ns sym val))
  (for ([(sym proc) (in-hash core-forms)])
    (namespace-set-transformer! ns sym (core-form proc))))

;; Helper for recognizing and dispatching on core forms:
(define (core-form-sym s)
  (define m (try-match-syntax s '(id . _)))
  (and m
       (let ([b (resolve (m 'id))])
         (and (top-level-binding? b)
              (top-level-binding-sym b)))))

