#lang racket/base
(require "scope.rkt"
         "binding.rkt"
         "core.rkt")

(provide make-lift-context
         add-lifted!
         get-and-clear-lifts!
         make-local-lift
         
         make-module-lift-context
         get-and-clear-module-lifts!
         add-lifted-module!)

;; ----------------------------------------

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

;; ----------------------------------------

(struct module-lift-context (lifts         ; box of list of lifted
                             module*-ok?)) ; whether `module*` is allowed

(define (make-module-lift-context module*-ok?)
  (module-lift-context (box null) module*-ok?))

(define (get-and-clear-module-lifts! module-lifts)
  (define l (unbox (module-lift-context-lifts module-lifts)))
  (set-box! (module-lift-context-lifts module-lifts) null)
  (reverse l))

(define (add-lifted-module! module-lifts s phase)
  (unless (module-lift-context-module*-ok? module-lifts)
    (case (core-form-sym s phase)
      [(module*)
       (error "cannot lift `module*` outside of a module:" s)]))
  (set-box! (module-lift-context-lifts module-lifts)
            (cons s
                  (unbox (module-lift-context-lifts module-lifts)))))
