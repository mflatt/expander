#lang racket/base
(require (only-in "syntax.rkt"
                  syntax?
                  identifier?)
         "module-binding.rkt"
         "checked-syntax.rkt"
         (only-in "scope.rkt" add-scopes)
         "namespace.rkt"
         "core.rkt"
         "phase.rkt"
         "require+provide.rkt"
         "expand-context.rkt"
         (rename-in "expand.rkt" [expand expand-in-context])
         "expand-require.rkt"
         "compile.rkt"
         "eval-compiled-top.rkt"
         "eval-compiled-module.rkt"
         "module-path.rkt"
         "linklet.rkt"
         "bulk-binding.rkt"
         "contract.rkt")

(provide eval
         compile
         expand
         namespace-syntax-introduce
         
         namespace-require
         namespace-module-identifier
         dynamic-require)

;; This `eval` is suitable as an eval handler that will be called by
;; the `eval` and `eval-syntax` of '#%kernel
(define (eval s [ns (current-namespace)] [compile compile])
  (parameterize ([current-bulk-binding-fallback-registry
                  (namespace-bulk-binding-registry ns)])
    (define c (cond
               [(or (compiled-top? s)
                    (linklet-directory? s))
                s]
               [(and (syntax? s)
                     (or (compiled-top? (syntax-e s))
                         (linklet-directory? (syntax-e s))))
                (syntax-e s)]
               [else
                (compile s ns)]))
    (cond
     [(compiled-top? c)
      (eval-top-from-compiled-top c ns)]
     [else
      (define h (linklet-directory->hash c))
      (cond
       [(hash-ref h #"" #f)
        (eval-module c #:namespace ns)]
       [else
        (eval-top-from-linklet-directory c ns)])])))

;; This `compile` is suitable as a compile handler that will be called
;; by the `compile` and `compile-syntax` of '#%kernel
(define (compile given-s [ns (current-namespace)] [expand expand])
  (define s (expand given-s ns))
  (case (core-form-sym s (namespace-phase ns))
    [(module)
     (compile-module s (make-compile-context #:namespace ns))]
    [else
     (compile-top s (make-compile-context #:namespace ns))]))

;; This `expand` is suitable as an expand handler (if such a thing
;; existed) to be called by `expand` and `expand-syntax`.
(define (expand given-s [ns (current-namespace)])
  (define s (maybe-intro given-s ns))
  (parameterize ([current-bulk-binding-fallback-registry
                  (namespace-bulk-binding-registry ns)])
    (expand-in-context s (make-expand-context ns))))

;; Add scopes to `s` if it's not syntax:
(define (maybe-intro s ns)
  (if (syntax? s)
      s
      (namespace-syntax-introduce (datum->syntax #f s) ns)))

(define (namespace-syntax-introduce s [ns (current-namespace)])
  (check 'namespace-syntax-introduce syntax? s)
  (check 'namespace-syntax-introduce namespace? ns)
  (define namespace-scopes (root-expand-context-module-scopes
                            (namespace-root-expand-ctx ns)))
  (define maybe-module-id
    (and (pair? (syntax-e s))
         (identifier? (car (syntax-e s)))
         (add-scopes (car (syntax-e s)) namespace-scopes)))
  (cond
   [(and maybe-module-id
         (free-identifier=? maybe-module-id
                            (namespace-module-identifier ns)))
    ;; The given syntax object starts `module`, so only add scope to `module`:
    (datum->syntax s (cons maybe-module-id (cdr (syntax-e s))) s s)]
   [else
    ;; Add scope everywhere:
    (add-scopes s namespace-scopes)]))

;; ----------------------------------------

(define (namespace-require req [ns (current-namespace)])
  (parse-and-perform-requires! #:run? #t
                               (list (add-scopes (datum->syntax #f req)
                                                 (root-expand-context-module-scopes
                                                  (namespace-root-expand-ctx ns))))
                               #f ns
                               (namespace-phase ns)
                               (make-requires+provides #f)))



(define (namespace-module-identifier [where (current-namespace)])
  (unless (or (namespace? where)
              (phase? where))
    (raise-argument-error 'namespace-module-identifier
                          (string-append "(or/c namespace? " phase?-string ")")
                          where))
  (datum->syntax (syntax-shift-phase-level core-stx
                                           (if (namespace? where)
                                               (namespace-phase where)
                                               where))
                 'module))

(define (dynamic-require mod-path sym [fail-k (lambda () (error "failed:" mod-path sym))])
  (unless (or (module-path? mod-path)
              (module-path-index? mod-path)
              (resolved-module-path? mod-path))
    (raise-argument-error 'dynamic-require
                          "(or/c module-path? module-path-index? resolved-module-path?)"
                          mod-path))
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
  (namespace-module-instantiate! ns mpi phase)

  (define m (namespace->module ns mod-name))
  (define binding (hash-ref (hash-ref (module-provides m) 0 #hasheq())
                            sym
                            #f))
  (cond
   [(not binding) (fail-k)]
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
                               [(eq? t missing) (fail-k)]
                               [else
                                ;; expand in a fresh namespace
                                (define tmp-ns (make-empty-namespace ns))
                                (define name (resolved-module-path-name mod-name))
                                (define mod-path (if (path? name)
                                                     name
                                                     `(quote ,name)))
                                (namespace-require mod-path tmp-ns)
                                (eval sym tmp-ns)])))]))
