#lang racket/base
(require "syntax.rkt"
         "property.rkt"
         "to-list.rkt"
         "scope.rkt"
         (rename-in "taint.rkt"
                    [syntax-tainted? raw:syntax-tainted?]
                    [syntax-arm raw:syntax-arm]
                    [syntax-disarm raw:syntax-disarm]
                    [syntax-rearm raw:syntax-rearm]
                    [syntax-taint raw:syntax-taint])
         (only-in "../expand/syntax-local.rkt" syntax-local-phase-level)
         "../namespace/core.rkt"
         "../namespace/inspector.rkt"
         "../common/contract.rkt")

;; Provides public versions of taint-related syntax functions

(provide syntax-tainted?
         syntax-arm
         syntax-disarm
         syntax-rearm
         syntax-taint)

(define (syntax-tainted? s)
  (check 'syntax-tainted? syntax? s)
  (raw:syntax-tainted? s))

(define (syntax-arm s [maybe-insp #f] [use-mode? #f])
  (check 'syntax-arm syntax? s)
  (unless (or (not maybe-insp)
              (inspector? maybe-insp))
    (raise-argument-error 'syntax-arm "(or/c inspector? #f)" maybe-insp))
  (define insp (inspector-for-taint maybe-insp))
  (cond
   [use-mode?
    (taint-dispatch
     s
     (lambda (s) (raw:syntax-arm s insp)))]
   [else
    (raw:syntax-arm s insp)]))

(define (syntax-disarm s maybe-insp)
  (check 'syntax-disarm syntax? s)
  (unless (or (not maybe-insp)
              (inspector? maybe-insp))
    (raise-argument-error 'syntax-disarm "(or/c inspector? #f)" maybe-insp))
  (define insp (inspector-for-taint maybe-insp))
  (raw:syntax-disarm s insp))
  
(define (syntax-rearm s from-s [use-mode? #f])
  (check 'syntax-disarm syntax? s)
  (check 'syntax-disarm syntax? from-s)
  (cond
   [use-mode? (taint-dispatch
               s
               (lambda (s) (raw:syntax-rearm s from-s)))]
   [else
    (raw:syntax-rearm s from-s)]))

(define (syntax-taint s)
  (check 'syntax-taint syntax? s)
  (raw:syntax-taint s))

;; ----------------------------------------

(define (inspector-for-taint maybe-insp)
  (or maybe-insp
      (current-module-code-inspector)
      (current-code-inspector)))

;; ----------------------------------------

(define (taint-dispatch s proc)
  (let loop ([s s] [mode (or (syntax-property s 'taint-mode)
                             (syntax-property s 'certify-mode))])
    (case mode
      [(opaque) (proc s)]
      [(transparent)
       (define c (syntax-map (or (syntax->list s)
                                 (syntax-e s))
                             (lambda (tail? d) d)
                             (lambda (s d) (loop s #f))
                             #f))
       (datum->syntax #f c s s)]
      [(transparent-binding)
       (define c (syntax-e s))
       (cond
        [(pair? c)
         (define cd (if (syntax? (cdr c))
                        (syntax-e (cdr c))
                        (cdr c)))
         (cond
          [(or (pair? cd)
               (and (syntax? cd) (pair? (syntax-e cd))))
           (define d (if (syntax? cd) (syntax-e cd) cd))
           (datum->syntax s
                          (cons (loop (car c) #f)
                                (cons (loop (car d) 'transparent)
                                      (syntax-map (cdr d)
                                                  (lambda (tail? d) d)
                                                  (lambda (s d) (loop s #f))
                                                  #f)))
                          s
                          s)]
          [else (loop s 'transparent)])]
        [else (loop s 'transparent)])]
      [else
       (define c (syntax-e s))
       (case (core-form-sym c (syntax-local-phase-level))
         [(begin module #%module-begin)
          (loop s 'transparent)]
         [(define-values define-syntaxes)
          (loop s 'transparent-binding)]
         [else
          (loop s 'opaque)])])))
