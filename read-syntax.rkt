#lang racket/base
(require "syntax.rkt"
         "datum-map.rkt"
         (prefix-in base:
                    (only-in racket/base
                             read-syntax
                             syntax? syntax-e syntax-property
                             syntax-source syntax-line syntax-column
                             syntax-position syntax-span)))

(provide read-syntax
         original-property-sym)

(define (read-syntax src [i (current-input-port)])
  (syntax->syntax (base:read-syntax src i)))

(define (syntax->syntax v)
  (datum-map v
             (lambda (tail? v)
               (cond
                [(base:syntax? v)
                 (struct-copy syntax empty-syntax
                              [content (syntax->syntax (base:syntax-e v))]
                              [srcloc (srcloc (base:syntax-source v)
                                              (base:syntax-line v)
                                              (base:syntax-column v)
                                              (base:syntax-position v)
                                              (base:syntax-span v))]
                              [props (case (base:syntax-property v 'paren-shape)
                                       [(#\[) original-square-props]
                                       [(#\{) original-curly-props]
                                       [else original-props])])]
                [else v]))))

(define original-property-sym
  (gensym 'original))

(define original-props
  (syntax-props (syntax-property empty-syntax original-property-sym #t)))
(define original-square-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\[)))
(define original-curly-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\{)))
