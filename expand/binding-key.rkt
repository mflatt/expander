#lang racket/base
(require "../syntax/binding.rkt")

;; A binding key is used by "requires+provides.rkt" as a quick way to
;; get from a binding to informaiton about the binding --- without
;; necessarily constructing or getting the binding itself. A fast path
;; is possible because, in a given module and at a given phase, each
;; symbol usually has a single imported binding

(provide make-binding-keys
         identifier->binding-key
         delayed-binding->binding-key)

(struct binding-keys (fast    ; phase -> symbol -> binding-or-(-> binding)
                      slow))  ; (cons phase module-binding) -> symbol

(define (make-binding-keys)
  (binding-keys (make-hasheqv) (make-hash)))

(define (identifier->binding-key bk id phase bind-sym)
  (delayed-binding->binding-key bk (lambda () (resolve+shift id phase)) phase bind-sym))

(define (delayed-binding->binding-key bk get-binding phase bind-sym)
  (define fast-keys (hash-ref (binding-keys-fast bk) phase #hasheq()))
  (cond
   [(hash-ref fast-keys bind-sym #f)
    => (lambda (current)
         (define current-b (if (procedure? current)
                               (let ([current-b (current)])
                                 (hash-set! (binding-keys-fast bk)
                                            phase
                                            (hash-set fast-keys bind-sym current-b))
                                 current-b)
                               current))
         (define b (if (procedure? get-binding)
                       (get-binding)
                       get-binding))
         (if (same-binding? current-b b)
             bind-sym
             ;; The relevant symbol is already used as a key, so select a different one
             (or (hash-ref (binding-keys-slow bk) (cons phase b) #f)
                 (let ([key (gensym)])
                   (hash-set! (binding-keys-slow bk) (cons phase b) key)
                   key))))]
   [else
    ;; Use just the symbol part as a key. A common case is that all
    ;; the symbols are different at a gven phase.
    (hash-set! (binding-keys-fast bk) phase (hash-set fast-keys bind-sym get-binding))
    bind-sym]))
