#lang racket/base
(require "phase.rkt"
         (rename-in "syntax.rkt"
                    [syntax->datum raw:syntax->datum]
                    [datum->syntax raw:datum->syntax])
         (rename-in "scope.rkt"
                    [syntax-e raw:syntax-e]
                    [bound-identifier=? raw:bound-identifier=?]
                    [syntax-shift-phase-level raw:syntax-shift-phase-level])
         (rename-in "binding.rkt"
                    [free-identifier=? raw:free-identifier=?]
                    [identifier-binding raw:identifier-binding]
                    [identifier-binding-symbol raw:identifier-binding-symbol])
         "syntax-local.rkt"
         "srcloc.rkt"
         "contract.rkt"
         (rename-in "debug.rkt"
                    [syntax-debug-info raw:syntax-debug-info]))

(provide syntax-e
         syntax->datum
         datum->syntax
         syntax->list
         bound-identifier=?
         free-identifier=?
         identifier-binding
         identifier-binding-symbol
         identifier-prune-lexical-context
         syntax-shift-phase-level
         syntax-track-origin
         syntax-debug-info)

(define (syntax-e s)
  (check 'syntax-e syntax? s)
  (raw:syntax-e s))

(define (syntax->datum s)
  (check 'syntax->datum syntax? s)
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
  (check syntax->list syntax? s)
  (define l
    (let loop ([s s])
      (cond
       [(pair? s) (cons (car s) (loop (cdr s)))]
       [(syntax? s) (loop (syntax-e s))]
       [else s])))
  (and (list? l)
       l))

(define (bound-identifier=? a b [phase (syntax-local-phase-level)])
  (check 'bound-identifier=? identifier? a)
  (check 'bound-identifier=? identifier? b)
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? phase?-string phase))
  (raw:bound-identifier=? a b phase))

(define (free-identifier=? a b
                           [a-phase (syntax-local-phase-level)]
                           [b-phase a-phase])
  (check 'free-identifier=? identifier? a)
  (check 'free-identifier=? identifier? b)
  (unless (phase? a-phase)
    (raise-argument-error 'free-identifier=? phase?-string a-phase))
  (unless (phase? b-phase)
    (raise-argument-error 'free-identifier=? phase?-string b-phase))
  (raw:free-identifier=? a b a-phase b-phase))

(define (identifier-binding id [phase (syntax-local-phase-level)])
  (check 'identifier-binding identifier? id)
  (unless (phase? phase)
    (raise-argument-error 'identifier-binding phase?-string phase))
  (raw:identifier-binding id phase))

(define (identifier-binding-symbol id [phase (syntax-local-phase-level)])
  (check 'identifier-binding-symbol identifier? id)
  (unless (phase? phase)
    (raise-argument-error 'identifier-binding-symbol phase?-string phase))
  (raw:identifier-binding-symbol id phase))

(define (identifier-prune-lexical-context id [syms null])
  (check 'identifier-prune-lexical-context identifier? id)
  (unless (and (list? syms)
               (andmap symbol? syms))
    (raise-argument-error 'identifier-prune-lexical-context "(listof symbol?)" syms))
  ;; It's a no-op in the Racket v6.5 expander
  id)

(define (syntax-debug-info s [phase (syntax-local-phase-level)] [all-bindings? #f])
  (check 'syntax-debug-info syntax? s)
  (unless (phase? phase)
    (raise-argument-error 'syntax-debug-info phase?-string phase))
  (raw:syntax-debug-info s phase all-bindings?))

(define (syntax-shift-phase-level s phase)
  (check 'syntax-shift-phase-level syntax? s)
  (unless (phase? phase)
    (raise-argument-error 'syntax-shift-phase-level phase?-string phase))
  (raw:syntax-shift-phase-level s phase))

(define (syntax-track-origin new-stx old-stx id)
  (check 'syntax-track-origin syntax? new-stx)
  (check 'syntax-track-origin syntax? old-stx)
  (check 'syntax-track-origin identifier? id)
  ;; No-op for now
  new-stx)
