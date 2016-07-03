#lang racket/base
(require "../syntax/module-binding.rkt"
         "../syntax/api.rkt"
         (only-in "../syntax/taint.rkt"
                  [syntax-disarm raw:syntax-disarm]
                  [syntax-rearm raw:syntax-rearm])
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/core.rkt"
         "../common/phase.rkt"
         "../syntax/match.rkt"
         "../expand/context.rkt"
         (rename-in "../expand/main.rkt" [expand expand-in-context])
         "../compile/main.rkt"
         "../compile/compiled-in-memory.rkt"
         "top.rkt"
         "module.rkt"
         "../common/module-path.rkt"
         "../host/linklet.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/contract.rkt"
         "../namespace/api.rkt"
         "../expand/lift-context.rkt"
         "../expand/require.rkt"
         "../expand/require+provide.rkt"
         "reflect.rkt"
         "../expand/log.rkt")

(provide eval
         compile
         expand
         expand-once
         expand-to-top-form

         compile-to-linklets)

;; This `eval` is suitable as an eval handler that will be called by
;; the `eval` and `eval-syntax` of '#%kernel. 
;; [Don't use keyword arguments here, because the function is
;;  exported for use by an embedding runtime system.]
(define (eval s [ns (current-namespace)] [compile (lambda (s ns)
                                                    (compile s ns #f))])
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
;; [Don't use keyword arguments here, because the function is
;;  exported for use by an embedding runtime system.]
(define (compile s [ns (current-namespace)] [serializable? #t] [expand expand] [to-source? #f])
  ;; The given `s` might be an already-compiled expression because it
  ;; went through some strange path, such as a `load` on a bytecode
  ;; file, which would wrap `#%top-interaction` around the compiled
  ;; expression where the expansion just discards the wrapper
  (define cs
    (cond
     [(compiled-expression? s) (list s)]
     [(and (syntax? s)
           (compiled-expression? (syntax-e s)))
      (list (syntax-e s))]
     [else
      (per-top-level s ns
                     #:single (lambda (s ns) (list (compile-single s ns expand
                                                              serializable?
                                                              to-source?)))
                     #:combine append)]))
  (if (= 1 (length cs))
      (car cs)
      (compiled-tops->compiled-top cs #:to-source? to-source?)))

;; Result is a hash table containing S-expressons that may have
;; "correlated" parts in the sense of "host/correlate.rkt"; use
;; `datum->correlated` plus `correlated->datum` to get a plain
;; S-expression
(define (compile-to-linklets s [ns (current-namespace)])
  (compile s ns #t expand #t))

(define (compile-single s ns expand serializable? to-source?)
  (define exp-s (expand s ns))
  (let loop ([exp-s exp-s])
    (define disarmed-exp-s (raw:syntax-disarm exp-s))
    (case (core-form-sym disarmed-exp-s (namespace-phase ns))
      [(module)
       (compile-module exp-s (make-compile-context #:namespace ns)
                       #:serializable? serializable?
                       #:to-source? to-source?)]
      [(begin)
       ;; expansion must have captured lifts
       (define-match m disarmed-exp-s '(begin e ...))
       (compiled-tops->compiled-top
        (for/list ([e (in-list (m 'e))])
          (loop e))
        #:to-source? to-source?)]
      [else
       (compile-top exp-s (make-compile-context #:namespace ns)
                    #:serializable? serializable?
                    #:to-source? to-source?)])))

;; This `expand` is suitable as an expand handler (if such a thing
;; existed) to be called by `expand` and `expand-syntax`.
;; [Don't use keyword arguments here, because the function is
;;  exported for use by an embedding runtime system.]
(define (expand s [ns (current-namespace)] [log-expand? #f])
  (when log-expand? (log-expand-start))
  (per-top-level s ns
                 #:single expand-single
                 #:combine cons
                 #:wrap re-pair))

(define (expand-single s ns)
  (define-values (require-lifts lifts exp-s)
    (expand-capturing-lifts s (make-expand-context ns)))
  (cond
   [(and (null? require-lifts) (null? lifts)) exp-s]
   [else
    (wrap-lifts-as-begin (append require-lifts lifts)
                         #:adjust-form (lambda (form) (expand-single form ns))
                         exp-s
                         s (namespace-phase ns))]))

(define (expand-once s [ns (current-namespace)])
  (per-top-level s ns
                 #:single expand-single-once
                 #:combine cons
                 #:wrap re-pair
                 #:just-once? #t))

(define (expand-single-once s ns)
  (define-values (require-lifts lifts exp-s)
    (expand-capturing-lifts s (struct-copy expand-context (make-expand-context ns)
                                           [just-once? #t])))
  (cond
   [(and (null? require-lifts) (null? lifts)) exp-s]
   [else
    (wrap-lifts-as-begin (append require-lifts lifts)
                         exp-s
                         s (namespace-phase ns))]))

(define (expand-to-top-form s [ns (current-namespace)])
  ;; Use `per-top-level` for immediate expansion and lift handling,
  ;; but `#:single #f` makes it return immediately
  (per-top-level s ns #:single #f))

;; ----------------------------------------

;; Top-level compilation and evaluation, which involves partial
;; expansion to detect `begin` and `begin-for-syntax` to interleave
;; expansions
(define (per-top-level given-s ns
                       #:single single        ; handle discovered form; #f => stop after immediate
                       #:combine [combine #f] ; how to cons a recur result, or not
                       #:wrap [wrap #f]       ; how to wrap a list of recur results, or not
                       #:just-once? [just-once? #f]) ; single expansion step
  (define s (maybe-intro given-s ns))
  (define ctx (make-expand-context ns))
  (define phase (namespace-phase ns))
  (let loop ([s s] [phase phase] [ns ns])
    (define tl-ctx (struct-copy expand-context ctx
                                [phase phase]
                                [namespace ns]
                                [just-once? just-once?]))
    (define-values (require-lifts lifts exp-s)
      (expand-capturing-lifts s (struct-copy expand-context tl-ctx
                                             [only-immediate? #t]
                                             [phase phase]
                                             [namespace ns])))
    (define disarmed-exp-s (raw:syntax-disarm exp-s))
    (cond
     [(or (pair? require-lifts) (pair? lifts))
      ;; Fold in lifted definitions and try again
      (define new-s (wrap-lifts-as-begin (append require-lifts lifts)
                                         exp-s
                                         s phase))
      (if just-once?
          new-s
          (loop new-s phase ns))]
     [(not single) exp-s]
     [(and just-once? (not (eq? exp-s s))) exp-s]
     [else
      (case (core-form-sym disarmed-exp-s phase)
        [(begin)
         (define-match m disarmed-exp-s '(begin e ...))
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
         (define-match m disarmed-exp-s '(begin-for-syntax e ...))
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
         (single exp-s ns)])])))

;; Add scopes to `s` if it's not syntax:
(define (maybe-intro s ns)
  (if (syntax? s)
      s
      (namespace-syntax-introduce (datum->syntax #f s) ns)))

(define (re-pair form-id s r)
  (raw:syntax-rearm
   (datum->syntax (raw:syntax-disarm s)
                  (cons form-id r)
                  s
                  s)
   s))

;; ----------------------------------------

(define (expand-capturing-lifts s ctx)
  (define ns (expand-context-namespace ctx))
  (namespace-visit-available-modules! ns)
  
  (define lift-ctx (make-lift-context (make-top-level-lift ctx)))
  (define require-lift-ctx (make-require-lift-context
                            (namespace-phase ns)
                            (make-parse-top-lifted-require ns)))
  (define exp-s
    (expand-in-context s (struct-copy expand-context ctx
                                      [lifts lift-ctx]
                                      [module-lifts lift-ctx]
                                      [require-lifts require-lift-ctx])))
  (values (get-and-clear-require-lifts! require-lift-ctx)
          (get-and-clear-lifts! lift-ctx)
          exp-s))

(define (make-parse-top-lifted-require ns)
  (lambda (s phase)
    ;; We don't "hide" this require in the same way as
    ;; a top-level `#%require`, because it's already
    ;; hidden in the sense of having an extra scope
    (define-match m (raw:syntax-disarm s) '(#%require req))
    (parse-and-perform-requires! (list (m 'req)) s
                                 ns phase #:run-phase phase
                                 (make-requires+provides #f))))
