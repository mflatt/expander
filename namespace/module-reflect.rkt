#lang racket/base
(require "../common/module-path.rkt"
         "../expand/root-expand-context.rkt"
         "namespace.rkt"
         (submod "namespace.rkt" for-module)
         "module.rkt"
         (submod "module.rkt" for-module-reflect))

(provide module-declared?
         module->language-info
         module->namespace)

;; ----------------------------------------

(define (module-declared? mod [load? #f])
  (unless (module-reference? mod)
    (raise-argument-error 'module-declared? module-reference-str mod))
  (define ns (current-namespace))
  (define name (reference->resolved-module-path mod #:load? load?))
  (and (namespace->module ns name) #t))

(define (module->language-info mod [load? #f])
  (unless (module-reference? mod)
    (raise-argument-error 'module->language-info module-reference-str mod))
  (define m (namespace->module/complain 'module->lanuage-info
                                        (current-namespace)
                                        (reference->resolved-module-path mod #:load? load?)))
  (module-language-info m))

(define (module->namespace mod)
  (unless (module-reference? mod)
    (raise-argument-error 'module->namespace module-reference-str mod))
  (define name (reference->resolved-module-path mod #:load? #t))
  (define ns (current-namespace))
  (define m-ns (namespace->module-namespace  ns name (namespace-phase ns)))
  (unless m-ns
    ;; Check for declaration:
    (namespace->module/complain 'module->namespace ns name)
    ;; Must be declared, but not instantiated
    (raise-arguments-error 'module->namespace
                           "module not instantiated in the current namespace"
                           "name" name))
  (unless (namespace-get-root-expand-ctx m-ns)
    ;; Instantiating the module didn't install a context, so make one now
    (namespace-set-root-expand-ctx! m-ns (make-root-expand-context)))
  m-ns)

;; ----------------------------------------

(define (namespace->module/complain who ns name)
  (or (namespace->module ns name)
      (raise-arguments-error who
                             "unknown module in the current namespace"
                             "name" name)))

;; ----------------------------------------
  
(define (module-reference? mod)
  (or (module-path? mod)
      (module-path-index? mod)
      (resolved-module-path? mod)))

(define module-reference-str
  "(or/c module-path? module-path-index? resolved-module-path?)")

(define (reference->resolved-module-path mod #:load? load?)
  (cond
   [(resolved-module-path? mod) mod]
   [else
    (define mpi (if (module-path-index? mod)
                    mod
                    (module-path-index-join mod #f)))
    (module-path-index-resolve mpi load?)]))
