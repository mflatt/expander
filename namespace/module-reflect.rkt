#lang racket/base
(require "../common/module-path.rkt"
         "namespace.rkt"
         (submod "namespace.rkt" for-module)
         (submod "module.rkt" for-module-reflect))

(provide module-declared?
         module->language-info)

;; ----------------------------------------

(define (module-declared? mod [load? #f])
  (unless (module-reference? mod)
    (raise-argument-error 'module-declared? module-reference-str mod))
  (define ns (current-namespace))
  (define name (reference->resolved-module-path mod load?))
  (and (namespace->module ns name) #t))

(define (module->language-info mod [load? #f])
  (unless (module-reference? mod)
    (raise-argument-error 'module->language-info module-reference-str mod))
  (define m (namespace->module/complain 'module->lanuage-info
                                        (current-namespace)
                                        (reference->resolved-module-path mod load?)))
  (module-language-info m))

;; ----------------------------------------

(define (namespace->module/complain who ns name)
  (or (namespace->module ns name)
      (raise-argument-error who
                            "unknown module in the current namespace"
                            "name" name)))

;; ----------------------------------------
  
(define (module-reference? mod)
  (or (module-path? mod)
      (module-path-index? mod)
      (resolved-module-path? mod)))

(define module-reference-str
  "(or/c module-path? module-path-index? resolved-module-path?)")

(define (reference->resolved-module-path mod load?)
  (cond
   [(resolved-module-path? mod) mod]
   [else
    (define mpi (if (module-path-index? mod)
                    mod
                    (module-path-index-join mod #f)))
    (module-path-index-resolve mpi load?)]))
