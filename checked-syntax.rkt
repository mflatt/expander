#lang racket/base
(require "phase.rkt"
         (rename-in "syntax.rkt"
                    [syntax->datum raw:syntax->datum]
                    [datum->syntax raw:datum->syntax])
         (rename-in "scope.rkt"
                    [bound-identifier=? raw:bound-identifier=?])
         (rename-in "binding.rkt"
                    [free-identifier=? raw:free-identifier=?]
                    [identifier-binding raw:identifier-binding]
                    [identifier-binding-symbol raw:identifier-binding-symbol])
         "syntax-local.rkt"
         "srcloc.rkt")

(provide syntax->datum
         datum->syntax
         syntax->list
         bound-identifier=?
         free-identifier=?
         identifier-binding
         identifier-binding-symbol)

(define (syntax->datum s)
  (unless (syntax? s)
    (raise-argument-error 'syntax->datum "syntax?" s))
  (raw:syntax->datum s))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f] [ignored #f])
  (unless (or (not stx-c) (syntax? stx-c))
    (raise-argument-error 'datum->syntax "(or #f syntax?)" stx-c))
  (unless (or (not stx-l)
              (syntax? stx-l)
              (encoded-srcloc? stx-l))
    (raise-argument-error 'datum->syntax "(or #f syntax? ...)" stx-l))
  (unless (or (not stx-p) (syntax? stx-p))
    (raise-argument-error 'datum->syntax "(or #f syntax?)" stx-p))
  (raw:datum->syntax stx-c s (to-srcloc-stx stx-l) stx-p))

(define (syntax->list s)
  (unless (syntax? s)
    (raise-argument-error 'syntax->list "syntax?" s))
  (define l
    (let loop ([s s])
      (cond
       [(pair? s) (cons (car s) (loop (cdr s)))]
       [(syntax? s) (loop (syntax-e s))]
       [else s])))
  (and (list? l)
       l))

(define (bound-identifier=? a b [phase (syntax-local-phase-level)])
  (unless (identifier? a)
    (raise-argument-error 'bound-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'bound-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:bound-identifier=? a b phase))

(define (free-identifier=? a b [phase (syntax-local-phase-level)])
  (unless (identifier? a)
    (raise-argument-error 'free-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'free-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'free-identifier=? "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:free-identifier=? a b phase))

(define (identifier-binding id [phase (syntax-local-phase-level)])
  (unless (identifier? id)
    (raise-argument-error 'identifier-binding "identifier?" id))
  (unless (phase? phase)
    (raise-argument-error 'identifier-binding "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:identifier-binding id phase))

(define (identifier-binding-symbol id [phase (syntax-local-phase-level)])
  (unless (identifier? id)
    (raise-argument-error 'identifier-binding-symbol "identifier?" id))
  (unless (phase? phase)
    (raise-argument-error 'identifier-binding-symbol "(or/c exact-nonnegative-integer? #f)" phase))
  (raw:identifier-binding-symbol id phase))
