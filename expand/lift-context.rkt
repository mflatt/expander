#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "env.rkt"
         "../namespace/core.rkt"
         "../namespace/namespace.rkt"
         "root-expand-context.rkt"
         "context.rkt"
         "def-id.rkt")

(provide make-lift-context
         add-lifted!
         get-and-clear-lifts!
         
         make-local-lift
         make-toplevel-lift
         wrap-lifts-as-let
         wrap-lifts-as-begin
         
         make-module-lift-context
         get-and-clear-module-lifts!
         add-lifted-module!
         
         make-require-lift-context
         add-lifted-require!
         get-and-clear-require-lifts!
         
         make-to-module-lift-context
         make-shared-module-ends
         to-module-lift-context-end-as-expressions?
         get-and-clear-end-lifts!
         get-and-clear-provide-lifts!
         add-lifted-to-module-provide!
         add-lifted-to-module-end!)

;; ----------------------------------------

(define (box-cons! b v)
  (set-box! b (cons v (unbox b))))

(define (box-clear! b)
  (begin0
   (reverse (unbox b))
   (set-box! b null)))

;; ----------------------------------------

(struct lift-context (convert ; takes a list of ids and rhs to produce a lifted
                      lifts)) ; box of list of lifted
(struct lifted (ids rhs))

(define (make-lift-context convert)
  (lift-context convert (box null)))

(define (add-lifted! lifts ids rhs phase)
  (define-values (lifted-ids lifted) ((lift-context-convert lifts) ids rhs phase))
  (box-cons! (lift-context-lifts lifts) lifted)
  lifted-ids)

(define (get-and-clear-lifts! lifts)
  (box-clear! (lift-context-lifts lifts)))

(define (make-local-lift lift-env counter)
  (lambda (ids rhs phase)
    (for ([id (in-list ids)])
      (define key (add-local-binding! id phase counter))
      (set-box! lift-env (hash-set (unbox lift-env) key variable)))
    (values ids (list ids rhs))))

(define (make-toplevel-lift ctx)
  (lambda (ids rhs phase)
    ;; Add the namespace's post-expansion scope (i.e., the inside-edge
    ;; scope) so that the binding has a specific phase:
    (define post-scope
      (root-expand-context-post-expansion-scope
       (namespace-get-root-expand-ctx
        (expand-context-namespace ctx))))
    (define tl-ids (for/list ([id (in-list ids)])
                     (add-scope id post-scope)))
    ;; Bind the identifier:
    (select-defined-syms-and-bind!/ctx tl-ids ctx)
    (values tl-ids (list tl-ids rhs))))

(define (wrap-lifts-as-let lifts body s phase)
  (datum->syntax
   s
   (for/fold ([body body]) ([lift (in-list (reverse lifts))])
     (list (datum->syntax
            (syntax-shift-phase-level core-stx phase)
            'let-values)
           (list lift)
           body))))

(define (wrap-lifts-as-begin lifts body s phase
                             #:adjust-defn [adjust-defn values])
  (datum->syntax
   #f
   (cons (datum->syntax
          (syntax-shift-phase-level core-stx phase)
          'begin)
         (append
          (for/list ([lift (in-list lifts)])
            (define ids (car lift))
            (define rhs (cadr lift))
            (adjust-defn
             (datum->syntax
              #f
              (list (datum->syntax
                     (syntax-shift-phase-level core-stx phase)
                     'define-values)
                    ids
                    rhs))))
          (list body)))))

;; ----------------------------------------

(struct module-lift-context (lifts         ; box of list of lifted
                             module*-ok?)) ; whether `module*` is allowed

(define (make-module-lift-context module*-ok?)
  (module-lift-context (box null) module*-ok?))

(define (get-and-clear-module-lifts! module-lifts)
  (box-clear! (module-lift-context-lifts module-lifts)))

(define (add-lifted-module! module-lifts s phase)
  (unless (module-lift-context-module*-ok? module-lifts)
    (case (core-form-sym s phase)
      [(module*)
       (error "cannot lift `module*` outside of a module:" s)]))
  (box-cons! (module-lift-context-lifts module-lifts)
             s))

;; ----------------------------------------

(struct require-lift-context (do-require  ; callback to process a lifted require
                              requires))  ; records lifted requires

(define (make-require-lift-context do-require)
  (require-lift-context do-require (box null)))

(define (get-and-clear-require-lifts! require-lifts)
  (box-clear! (require-lift-context-requires require-lifts)))

(define (add-lifted-require! require-lifts s phase)
  ((require-lift-context-do-require require-lifts) s phase)
  (box-cons! (require-lift-context-requires require-lifts)
             s))

;; ----------------------------------------

(struct to-module-lift-context (provides
                                end-as-expressions?
                                ends))

(define (make-to-module-lift-context #:shared-module-ends ends
                                     #:end-as-expressions? end-as-expressions?)
  (to-module-lift-context (box null) 
                          end-as-expressions?
                          ends))

(define (make-shared-module-ends)
  (box null))

(define (get-and-clear-end-lifts! to-module-lifts)
  (box-clear! (to-module-lift-context-ends to-module-lifts)))

(define (get-and-clear-provide-lifts! to-module-lifts)
  (box-clear! (to-module-lift-context-provides to-module-lifts)))

(define (add-lifted-to-module-provide! to-module-lifts s phase)
  (box-cons! (to-module-lift-context-provides to-module-lifts)
             s))

(define (add-lifted-to-module-end! to-module-lifts s phase)
  (box-cons! (to-module-lift-context-ends to-module-lifts)
             s))
