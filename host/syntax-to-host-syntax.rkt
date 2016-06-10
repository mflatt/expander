#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         (only-in racket/base
                  [datum->syntax host:datum->syntax]
                  [syntax-property host:syntax-property]))

(provide syntax->host-syntax)

(define (syntax->host-syntax v)
  (syntax-map v
              (lambda (tail? v) v)
              (lambda (orig-s d)
                (define s (host:datum->syntax #f d (srcloc->vector (syntax-srcloc orig-s))))
                (define keys (syntax-property-symbol-keys orig-s))
                (for/fold ([s s]) ([key (in-list keys)])
                  (host:syntax-property s key (syntax-property orig-s key))))
              syntax-e))

(define (srcloc->vector s)
  (and s
       (vector (srcloc-source s)
               (srcloc-line s)
               (srcloc-column s)
               (srcloc-position s)
               (srcloc-span s))))
