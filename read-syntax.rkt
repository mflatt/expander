#lang racket/base
(require "checked-syntax.rkt"
         "datum-map.rkt"
         (prefix-in base:
                    (only-in racket/base
                             read-syntax
                             syntax? syntax-e
                             syntax-source syntax-line syntax-column
                             syntax-position syntax-span)))

(provide read-syntax)

(define (read-syntax src [i (current-input-port)])
  (syntax->syntax (base:read-syntax src i)))

(define (syntax->syntax v)
  (datum-map v
             (lambda (tail? v)
               (cond
                [(base:syntax? v)
                 (datum->syntax #f
                                (syntax->syntax (base:syntax-e v))
                                (vector (base:syntax-source v)
                                        (base:syntax-line v)
                                        (base:syntax-column v)
                                        (base:syntax-position v)
                                        (base:syntax-span v)))]
                [else v]))))
