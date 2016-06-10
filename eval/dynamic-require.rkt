#lang racket/base
(require "../common/phase.rkt"
         "../syntax/module-binding.rkt"
         "../syntax/checked-syntax.rkt"
         "../namespace/namespace.rkt"
         "../common/module-path.rkt"
         "../namespace/eval.rkt"
         "eval.rkt")

(provide dynamic-require)

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
     [else (module-path-index-join (resolved-module-path->module-path mod-path) #f)]))
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
                                (define t (namespace-get-transformer m-ns ex-phase ex-sym missing))
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
                                  (define tmp-ns (make-namespace ns))
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

