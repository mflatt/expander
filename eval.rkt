#lang racket/base
(require "module-binding.rkt"
         "checked-syntax.rkt"
         "namespace.rkt"
         "core.rkt"
         "phase.rkt"
         "match.rkt"
         "expand-context.rkt"
         (rename-in "expand.rkt" [expand expand-in-context])
         "compile.rkt"
         "compiled-in-memory.rkt"
         "eval-compiled-top.rkt"
         "eval-compiled-module.rkt"
         "module-path.rkt"
         "linklet.rkt"
         "bulk-binding.rkt"
         "contract.rkt"
         "namespace-eval.rkt")

(provide eval
         compile
         expand

         compiled-module-expression?
         
         dynamic-require)

;; This `eval` is suitable as an eval handler that will be called by
;; the `eval` and `eval-syntax` of '#%kernel
(define (eval s [ns (current-namespace)] [compile compile])
  (parameterize ([current-bulk-binding-fallback-registry
                  (namespace-bulk-binding-registry ns)])
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
                                (eval-compiled (compile s ns) ns)))])))

(define (eval-compiled c ns)
  (cond
   [(compiled-module-expression? c)
    (eval-module c #:namespace ns)]
   [else
    (eval-top c ns eval-compiled)]))

(define (compiled-module-expression? c)
  (define ld (if (compiled-in-memory? c)
                 (compiled-in-memory-linklet-directory c)
                 c))
  (and (linklet-directory? ld)
       (hash-ref (linklet-directory->hash ld) #".decl" #f)))

;; This `compile` is suitable as a compile handler that will be called
;; by the `compile` and `compile-syntax` of '#%kernel
(define (compile s [ns (current-namespace)] [expand expand]
                 #:serializable? [serializable? #f])
  (define cs
    (parameterize ([current-bulk-binding-fallback-registry
                  (namespace-bulk-binding-registry ns)])
      (per-top-level s ns
                     #:single (lambda (s ns) (list (compile-single s ns expand
                                                              serializable?)))
                     #:combine append)))
  (if (= 1 (length cs))
      (car cs)
      (compiled-tops->compiled-top cs)))

(define (compile-single s ns expand serializable?)
  (define exp-s (expand s ns))
  (case (core-form-sym exp-s (namespace-phase ns))
    [(module)
     (compile-module exp-s (make-compile-context #:namespace ns))]
    [else
     (compile-top exp-s (make-compile-context #:namespace ns)
                  #:serializable? serializable?)]))

;; This `expand` is suitable as an expand handler (if such a thing
;; existed) to be called by `expand` and `expand-syntax`.
(define (expand s [ns (current-namespace)])
  (parameterize ([current-bulk-binding-fallback-registry
                  (namespace-bulk-binding-registry ns)])
    (per-top-level s ns
                   #:single expand-single
                   #:combine cons
                   #:wrap (lambda (form-id s r)
                            (datum->syntax s
                                           (cons form-id r)
                                           s
                                           s)))))

(define (expand-single s ns)
  (expand-in-context s (make-expand-context ns)))

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
    (define exp-s
      (expand-in-context s (struct-copy expand-context ctx
                                        [only-immediate? #t]
                                        [phase phase]
                                        [namespace ns])))
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
       (define l
         (for/list ([s (in-list (m 'e))])
           (loop s next-phase next-ns)))
       (cond
        [wrap (wrap (m 'begin-for-syntax) exp-s l)]
        [combine l]
        [else (void)])]
      [else
       (single exp-s ns)])))

;; ----------------------------------------

(define (dynamic-require mod-path sym [fail-k default-fail-thunk])
  (unless (or (module-path? mod-path)
              (module-path-index? mod-path)
              (resolved-module-path? mod-path))
    (raise-argument-error 'dynamic-require
                          "(or/c module-path? module-path-index? resolved-module-path?)"
                          mod-path))
  (unless (or (symbol? sym)
              (not sym)
              (equal? sym 0)
              (void? sym))
    (raise-argument-error 'dynamic-require "(or/c symbol? #f 0 void?)" sym))
  (unless (and (procedure? fail-k) (procedure-arity-includes? fail-k 0))
    (raise-argument-error 'dynamic-require "(-> any)" fail-k))
  (define ns (current-namespace))
  (define mpi
    (cond
     [(module-path? mod-path) (module-path-index-join mod-path #f)]
     [(module-path-index? mod-path) mod-path]
     [else
      (define name (resolved-module-path-name mod-path))
      (define root-name (if (pair? name) (car name) name))
      (define root-mod-path (if (path? root-name)
                                root-name
                                `(quote ,root-name)))
      (define new-mod-path (if (pair? name)
                               root-mod-path
                               `(submod ,root-mod-path ,@(cdr name))))
      (module-path-index-join new-mod-path #f)]))
  (define mod-name (module-path-index-resolve mpi #t))
  (define phase (namespace-phase ns))
  (cond
   [(or (not sym)
        (equal? sym 0)) ;; FIXME: 0 is different when we distinguish availability
    (namespace-module-instantiate! ns mpi phase)]
   [(void? sym)
    (namespace-module-visit! ns mpi phase)]
   [else
    (namespace-module-instantiate! ns mpi phase)
    (define m (namespace->module ns mod-name))
    (define binding (hash-ref (hash-ref (module-provides m) 0 #hasheq())
                              sym
                              #f))
    (cond
     [(not binding) (if (eq? fail-k default-fail-thunk)
                        (raise-arguments-error 'dynamic-require
                                               "name is not provided"
                                               "name" sym
                                               "module" mod-name)
                        (fail-k))]
     [else
      (define ex-sym (module-binding-sym binding))
      (define ex-phase (module-binding-phase binding))
      (define m-ns (namespace->module-namespace ns
                                                (module-path-index-resolve
                                                 (module-path-index-shift
                                                  (module-binding-module binding)
                                                  (module-self m)
                                                  mpi))
                                                (phase- phase ex-phase)
                                                #:complain-on-failure? #t))
      (namespace-get-variable m-ns ex-phase ex-sym
                              (lambda ()
                                ;; Maybe syntax?
                                (define missing (gensym 'missing))
                                (define t (namespace-get-transformer m-ns ex-phase sym missing))
                                (cond
                                 [(eq? t missing)
                                  (if (eq? fail-k default-fail-thunk)
                                      (raise-arguments-error 'dynamic-require
                                                             "name is not provided"
                                                             "name" sym
                                                             "module" mod-name)
                                      (fail-k))]
                                 [else
                                  ;; expand in a fresh namespace
                                  (define tmp-ns (make-empty-namespace ns))
                                  (define name (resolved-module-path-name mod-name))
                                  (define mod-path (if (path? name)
                                                       name
                                                       `(quote ,name)))
                                  (namespace-require mod-path tmp-ns)
                                  (eval sym tmp-ns)])))])]))

;; The `dynamic-require` function checks by recognizing this failure
;; thunk and substituting a more specific error:
(define (default-fail-thunk)
  (error "failed"))

