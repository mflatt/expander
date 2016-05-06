#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         (rename-in "scope.rkt"
                    [bound-identifier=? raw:bound-identifier=?])
         "namespace.rkt"
         (rename-in "binding.rkt"
                    [free-identifier=? raw:free-identifier=?])
         "core.rkt"
         "expand-context.rkt")

;; `bound-identifier=?` and `free-identifier=?` use the current
;; context to determine the default phase
(define (default-phase)
  (define ctx (current-expand-context))
  (if ctx
      (expand-context-phase ctx)
      0))

(define (bound-identifier=? a b [phase (default-phase)])
  (unless (identifier? a)
    (raise-argument-error 'bound-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'bound-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:bound-identifier=? a b phase))

(define (free-identifier=? a b [phase (default-phase)])
  (unless (identifier? a)
    (raise-argument-error 'free-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'free-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'free-identifier=? "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:free-identifier=? a b phase))

;; Register core primitives:
;; This list will need to be a lot longer...
(add-core-primitive! 'syntax-e syntax-e)
(add-core-primitive! 'datum->syntax datum->syntax)
(add-core-primitive! 'bound-identifier=? bound-identifier=?)
(add-core-primitive! 'free-identifier=? free-identifier=?)
(add-core-primitive! 'cons cons)
(add-core-primitive! 'list list)
(add-core-primitive! 'car car)
(add-core-primitive! 'cdr cdr)
(add-core-primitive! 'null? null?)
(add-core-primitive! 'map map)
(add-core-primitive! 'values values)
(add-core-primitive! 'println println)
(add-core-primitive! 'random random)
(add-core-primitive! '+ +)
