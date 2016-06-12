#lang racket/base
(require "../syntax/module-binding.rkt"
         "../syntax/checked-syntax.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/core.rkt"
         "../common/phase.rkt"
         "../syntax/match.rkt"
         "../expand/context.rkt"
         (rename-in "../expand/expand.rkt" [expand expand-in-context])
         "../compile/main.rkt"
         "../compile/compiled-in-memory.rkt"
         "compiled-top.rkt"
         "compiled-module.rkt"
         "../common/module-path.rkt"
         "../host/linklet.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/contract.rkt"
         "../namespace/eval.rkt"
         "../expand/lift-context.rkt"
         "compiled-reflect.rkt")

(provide eval
         compile
         expand)

;; This `eval` is suitable as an eval handler that will be called by
;; the `eval` and `eval-syntax` of '#%kernel
(define (eval s [ns (current-namespace)] [compile (lambda (s ns)
                                                    (compile s ns  #:serializable? #f))])
  (cond
   [(or (compiled-in-memory? s)
        (linklet-directory? s))
    (eval-compiled s ns)]
   [(and (syntax? s)
         (or (compiled-in-memory? (syntax-e s))
             (linklet-directory? (syntax-e s))))
    (eval-compiled (syntax->datum s) ns)]
   [else
    (per-top-level s ns 
                   #:single (lambda (s ns)
                              (eval-compiled (compile s ns) ns)))]))

(define (eval-compiled c ns)
  (cond
   [(compiled-module-expression? c)
    (eval-module c #:namespace ns)]
   [else
    (eval-top c ns eval-compiled)]))

;; This `compile` is suitable as a compile handler that will be called
;; by the `compile` and `compile-syntax` of '#%kernel
(define (compile s [ns (current-namespace)] [expand expand]
                 #:serializable? [serializable? #t])
  (define cs
    (per-top-level s ns
                   #:single (lambda (s ns) (list (compile-single s ns expand
                                                            serializable?)))
                   #:combine append))
  (if (= 1 (length cs))
      (car cs)
      (compiled-tops->compiled-top cs)))

(define (compile-single s ns expand serializable?)
  (define exp-s (expand s ns))
  (case (core-form-sym exp-s (namespace-phase ns))
    [(module)
     (compile-module exp-s (make-compile-context #:namespace ns)
                     #:serializable? serializable?)]
    [(begin)
     ;; expansion must have captured lifts
     (define m (match-syntax exp-s '(begin e ...)))
     (compiled-tops->compiled-top
      (for/list ([e (in-list (m 'e))])
        (compile-top e (make-compile-context #:namespace ns)
                     #:serializable? serializable?)))]
    [else
     (compile-top exp-s (make-compile-context #:namespace ns)
                  #:serializable? serializable?)]))

;; This `expand` is suitable as an expand handler (if such a thing
;; existed) to be called by `expand` and `expand-syntax`.
(define (expand s [ns (current-namespace)])
  (per-top-level s ns
                 #:single expand-single
                 #:combine cons
                 #:wrap (lambda (form-id s r)
                          (datum->syntax s
                                         (cons form-id r)
                                         s
                                         s))))

(define (expand-single s ns)
  (namespace-visit-available-modules! ns)
  ;; Ideally, the rest would be just
  ;; (expand-in-context s (make-expand-context ns))
  ;; but we have to handle lifted definitions
  (define ctx (make-expand-context ns))
  (define lift-ctx (make-lift-context (make-toplevel-lift ctx)))
  (define exp-s
    (expand-in-context s (struct-copy expand-context ctx
                                      [lifts lift-ctx])))
  (define lifts (get-and-clear-lifts! lift-ctx))
  (cond
   [(null? lifts) exp-s]
   [else
    (wrap-lifts-as-begin lifts exp-s s (namespace-phase ns)
                         #:adjust-defn
                         (lambda (defn)
                           (expand-single defn ns)))]))

;; Add scopes to `s` if it's not syntax:
(define (maybe-intro s ns)
  (if (syntax? s)
      s
      (namespace-syntax-introduce (datum->syntax #f s) ns)))

;; Top-level compilation and evaluation, which involves partial
;; expansion to detect `begin` and `begin-for-syntax` to interleave
;; expansions
(define (per-top-level given-s ns
                       #:single single
                       #:combine [combine #f]
                       #:wrap [wrap #f])
  (define s (maybe-intro given-s ns))
  (define ctx (make-expand-context ns))
  (define phase (namespace-phase ns))
  (let loop ([s s] [phase phase] [ns ns])
    (define tl-ctx (struct-copy expand-context ctx
                                [phase phase]
                                [namespace ns]))
    (namespace-visit-available-modules! ns)
    (define lift-ctx (make-lift-context (make-toplevel-lift tl-ctx)))
    (define exp-s
      (expand-in-context s (struct-copy expand-context tl-ctx
                                        [only-immediate? #t]
                                        [phase phase]
                                        [namespace ns]
                                        [lifts lift-ctx])))
    (define lifts (get-and-clear-lifts! lift-ctx))
    (cond
     [(null? lifts)
      (case (core-form-sym exp-s phase)
        [(begin)
         (define m (match-syntax exp-s '(begin e ...)))
         ;; Map `loop` over the `e`s, but in the case of `eval`,
         ;; tail-call for last one:
         (define (begin-loop es)
           (cond
            [(null? es) (if combine null (void))]
            [(and (not combine) (null? (cdr es)))
             (loop (car es) phase ns)]
            [else
             (define a (loop (car es) phase ns))
             (if combine
                 (combine a (begin-loop (cdr es)))
                 (begin-loop (cdr es)))]))
         (if wrap
             (wrap (m 'begin) exp-s (begin-loop (m 'e)))
             (begin-loop (m 'e)))]
        [(begin-for-syntax)
         (define m (match-syntax exp-s '(begin-for-syntax e ...)))
         (define next-phase (add1 phase))
         (define next-ns (namespace->namespace-at-phase ns next-phase))
         (namespace-visit-available-modules! next-ns) ; to match old behavior for empty body
         (define l
           (for/list ([s (in-list (m 'e))])
             (loop s next-phase next-ns)))
         (cond
          [wrap (wrap (m 'begin-for-syntax) exp-s l)]
          [combine l]
          [else (void)])]
        [else
         (single exp-s ns)])]
     [else
      ;; Fold in lifted definitions and try again
      (define new-s (wrap-lifts-as-begin lifts exp-s s phase))
      (loop new-s phase ns)])))
