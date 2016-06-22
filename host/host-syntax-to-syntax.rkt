#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/original.rkt"
         "../syntax/datum-map.rkt"
         (prefix-in host:
                    (only-in "syntax.rkt"
                             syntax? syntax-e syntax-property syntax-property-preserved?
                             syntax-property-symbol-keys
                             syntax-source syntax-line syntax-column
                             syntax-position syntax-span)))

(provide host-syntax->syntax)

(define (host-syntax->syntax v)
  (datum-map v
             (lambda (tail? v)
               (cond
                [(host:syntax? v)
                 (define s
                   (struct-copy syntax empty-syntax
                                [content (host-syntax->syntax (host:syntax-e v))]
                                [srcloc (srcloc (host:syntax-source v)
                                                (host:syntax-line v)
                                                (host:syntax-column v)
                                                (host:syntax-position v)
                                                (host:syntax-span v))]
                                [props (case (host:syntax-property v 'paren-shape)
                                         [(#\[) original-square-props]
                                         [(#\{) original-curly-props]
                                         [else original-props])]))
                 (define keys (host:syntax-property-symbol-keys v))
                 (cond
                  [(null? keys) s]
                  [(and (null? (cdr keys)) (eq? (car keys) 'paren-shape)) s]
                  [else (for/fold ([s s]) ([key (in-list keys)])
                          (syntax-property s key (host:syntax-property v key)
                                           (host:syntax-property-preserved? v key)))])]
                [else v]))))

(define original-props
  (syntax-props (syntax-property empty-syntax original-property-sym #t)))
(define original-square-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\[)))
(define original-curly-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\{)))
