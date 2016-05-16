#lang racket/base

(provide phase?
         phase+
         phase-
         phase?-string)

;; Terminology:
;;
;;  * A "phase" is the phase at which a module is instantiated.
;;
;;  * A "phase level" is a phase relative to a module's body.
;;
;;  * A "phase shift" is a phase to combne with other phases.
;;
;; This termonology is approximate, because one use's "phase" is
;; another use's "phase level".

(define (phase? v)
  (or (not v)
      (exact-integer? v)))

(define (phase+ a b)
  (and a b (+ a b)))

(define (phase- a b)
  (and a b (- a b)))

;; For contract errors:
(define phase?-string "(or/c exact-integer? #f)")
