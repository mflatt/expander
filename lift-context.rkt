#lang racket/base
(require "scope.rkt"
         "binding.rkt")

(provide make-lift-context
         add-lifted!
         get-and-clear-lifts!
         make-local-lift)

(struct lift-context (convert ; takes a list of ids and rhs to produce a lifted
                      lifts)) ; box of list of lifted
(struct lifted (ids rhs))

(define (make-lift-context convert)
  (lift-context convert (box null)))

(define (add-lifted! lifts ids rhs phase)
  (define-values (lifted-ids lifted) ((lift-context-convert lifts) ids rhs phase))
  (set-box! (lift-context-lifts lifts)
            (cons lifted
                  (unbox (lift-context-lifts lifts))))
  lifted-ids)

(define (get-and-clear-lifts! lifts)
  (define l (unbox (lift-context-lifts lifts)))
  (set-box! (lift-context-lifts lifts) null)
  (reverse l))

(define (make-local-lift lift-env)
  (lambda (ids rhs phase)
    (for ([id (in-list ids)])
      (define key (add-local-binding! id phase))
      (set-box! lift-env (hash-set (unbox lift-env) key variable)))
    (values ids (list ids rhs))))
