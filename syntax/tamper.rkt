#lang racket/base
(require "../common/set.rkt"
         "datum-map.rkt")

(provide tamper-tainted?
         tamper-armed?
         tamper-clean?
         tamper-tainted-for-content
         
         serialize-tamper
         deserialize-tamper
         current-arm-inspectors)

;; A tamper status is either
;;   - #f (clean)
;;   - 'tainted (tainted)
;;   - 'tainted/need-propagate *tainted, and status need to be propagated to children)
;;   - a set of inspectors (armed with a dye pack)

(define (tamper-tainted? v)
  (symbol? v))

(define (tamper-armed? v)
  (set? v))

(define (tamper-clean? v)
  (not v))

(define (tamper-tainted-for-content v)
  (if (datum-has-elements? v)
      'tainted/need-propagate
      'tainted))

;; ----------------------------------------

(define (serialize-tamper t)
  (if (tamper-armed? t) 'armed t))

;; Set during deserialize to select a code inspector:
(define current-arm-inspectors (make-parameter (seteq)))

(define (deserialize-tamper t)
  (if (eq? t 'armed) (current-arm-inspectors) t))
