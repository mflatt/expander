#lang racket/base
(require "../common/phase.rkt"
         "../syntax/module-binding.rkt"
         "../syntax/api.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/protect.rkt"
         "../common/module-path.rkt"
         "../namespace/api.rkt"
         "main.rkt")

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
  ;; Dispatch to the variant of `dynamic-require` that is determined
  ;; by the second argument:
  (cond
   [(not sym)
    ;; Run phase 0; don't visit or make available
    (namespace-module-instantiate! ns mpi phase #:run-phase phase
                                   #:otherwise-available? #f)]
   [(equal? sym 0)
    ;; Run phase 0, also make available
    (namespace-module-instantiate! ns mpi phase #:run-phase phase)]
   [(void? sym)
    ;; Just visit
    (namespace-module-visit! ns mpi phase #:visit-phase phase)]
   [else
    ;; Extract a particular value via phase 0....
    (define m (namespace->module ns mod-name))
    (define binding/maybe-protected (hash-ref (hash-ref (module-provides m) 0 #hasheq())
                                              sym
                                              #f))
    (cond
     [(not binding/maybe-protected)
      (if (eq? fail-k default-fail-thunk)
          (raise-arguments-error 'dynamic-require
                                 "name is not provided"
                                 "name" sym
                                 "module" mod-name)
          (fail-k))]
     [else
      ;; The provided binding may correspond to an immediate provide,
      ;; or it may by re-rpovided from a different module
      (define binding (if (protected? binding/maybe-protected)
                          (protected-binding binding/maybe-protected)
                          binding/maybe-protected))
      (define ex-sym (module-binding-sym binding))
      (define ex-phase (module-binding-phase binding))
      (namespace-module-instantiate! ns mpi phase #:run-phase phase)
      (define m-ns (namespace->module-namespace ns
                                                (module-path-index-resolve
                                                 (module-path-index-shift
                                                  (module-binding-module binding)
                                                  (module-self m)
                                                  mpi))
                                                (phase- phase ex-phase)
                                                #:complain-on-failure? #t))
      ;; Before continuing, make sure that we're allowed to access the binding
      (when (and (protected? binding/maybe-protected)
                 (and (not (inspector-superior? (current-code-inspector) (namespace-inspector m-ns)))
                      (not (and (module-binding-extra-inspector binding)
                                (inspector-superior? (module-binding-extra-inspector binding)
                                                     (namespace-inspector m-ns))))))
        (raise-arguments-error 'dynamc-require
                               "name is protected"
                               "name" sym
                               "module" mod-name))
      ;; Try to get an exported variable...
      (namespace-get-variable m-ns ex-phase ex-sym
                              (lambda ()
                                ;; No such variable; maybe it's syntax?
                                (define missing (gensym 'missing))
                                (namespace-module-visit! ns mpi phase #:visit-phase phase)
                                (define t (namespace-get-transformer m-ns ex-phase ex-sym missing))
                                (cond
                                 [(eq? t missing)
                                  ;; We can't find a transformer export, either
                                  (if (eq? fail-k default-fail-thunk)
                                      (raise-arguments-error 'dynamic-require
                                                             "name is not provided"
                                                             "name" sym
                                                             "module" mod-name)
                                      (fail-k))]
                                 [else
                                  ;; Found transformer; expand in a fresh namespace
                                  (define tmp-ns (make-namespace ns))
                                  (define mod-path (resolved-module-path->module-path mod-name))
                                  (namespace-require mod-path tmp-ns)
                                  (eval sym tmp-ns)])))])]))

;; The `dynamic-require` function cheats by recognizing this failure
;; thunk and substituting a more specific error:
(define (default-fail-thunk)
  (error "failed"))
