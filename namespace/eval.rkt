#lang racket/base
(require (only-in "../syntax/syntax.rkt" syntax-mpi-shifts)
         (only-in "../syntax/scope.rkt" add-binding!)
         (only-in "../syntax/binding.rkt" resolve+shift syntax-transfer-shifts)
         "../syntax/inspector.rkt"
         "../syntax/module-binding.rkt"
         "../syntax/checked-syntax.rkt"
         "../syntax/error.rkt"
         (only-in "../syntax/scope.rkt" add-scopes push-scope)
         "namespace.rkt"
         "module.rkt"
         "attach.rkt"
         "core.rkt"
         "../common/phase.rkt"
         "../expand/require+provide.rkt"
         "../expand/context.rkt"
         "../expand/require.rkt"
         "../common/module-path.rkt"
         "../common/contract.rkt"
         "../expand/protect.rkt"
         "../expand/env.rkt")

(provide make-empty-namespace
         
         namespace-syntax-introduce
         namespace-module-identifier
         namespace-symbol->identifier
          
         namespace-require
         namespace-require/copy
         namespace-require/constant
         namespace-require/expansion-time
         
         namespace-variable-value
         namespace-set-variable-value!
         namespace-undefine-variable!
         
         namespace-mapped-symbols

         namespace-base-phase)

(define (make-empty-namespace)
  (define current-ns (current-namespace))
  (define phase (namespace-phase current-ns))
  (define ns (namespace->namespace-at-phase (make-namespace)
                                            phase))
  ;; For historical reasons, an empty namespace isn't actually
  ;; empty; we always carry '#%kernel along
  (namespace-attach-module current-ns ''#%kernel ns)
  (namespace-primitive-module-visit! ns '#%kernel)
  ns)

(define (namespace-syntax-introduce s [ns (current-namespace)])
  (check 'namespace-syntax-introduce syntax? s)
  (check 'namespace-syntax-introduce namespace? ns)
  (define root-ctx (namespace-get-root-expand-ctx ns))
  (define post-scope (root-expand-context-post-expansion-scope root-ctx))
  (define other-namespace-scopes (for/list ([sc (in-list (root-expand-context-module-scopes root-ctx))]
                                            #:unless (equal? sc post-scope))
                                   sc))
  (define (add-ns-scopes s)
    (syntax-set-inspector
     (syntax-transfer-shifts (push-scope (add-scopes s other-namespace-scopes)
                                         post-scope)
                             (root-expand-context-all-scopes-stx root-ctx))
     (or (namespace-declaration-inspector ns)
         (current-code-inspector))))
  (define maybe-module-id
    (and (pair? (syntax-e s))
         (identifier? (car (syntax-e s)))
         (add-ns-scopes (car (syntax-e s)))))
  (cond
   [(and maybe-module-id
         (free-identifier=? maybe-module-id
                            (namespace-module-identifier ns)))
    ;; The given syntax object starts `module`, so only add scope to `module`:
    (datum->syntax s (cons maybe-module-id (cdr (syntax-e s))) s s)]
   [else
    ;; Add scope everywhere:
    (add-ns-scopes s)]))

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

(define (namespace-symbol->identifier sym)
  (check 'namespace-symbol->identifier symbol? sym)
  (namespace-syntax-introduce (datum->syntax #f sym)))

;; ----------------------------------------

(define (do-namespace-require #:run? run? #:visit? visit? who req ns)
  (check who namespace? ns)
  (parse-and-perform-requires! #:run? run?
                               #:visit? visit?
                               (list (add-scopes (datum->syntax #f req)
                                                 (root-expand-context-module-scopes
                                                  (namespace-get-root-expand-ctx ns))))
                               #f
                               ns
                               (namespace-phase ns)
                               (make-requires+provides #f)))

(define (namespace-require req [ns (current-namespace)])
  (do-namespace-require #:run? #t #:visit? #f 'namespace-require req ns))

(define (namespace-require/expansion-time req [ns (current-namespace)])
  (do-namespace-require #:run? #f #:visit? #t 'namespace-require/expansion-time req ns))
  
;; FIXME
(define (namespace-require/constant req [ns (current-namespace)])
  (namespace-require req ns))

;; FIXME
(define (namespace-require/copy req [ns (current-namespace)])
  (namespace-require req ns))

;; ----------------------------------------

(define (namespace-variable-value sym
                                  [use-mapping? #f]
                                  [failure-thunk #f]
                                  [ns (current-namespace)])
  (check 'namespace-variable-value symbol? sym)
  (unless (or (not failure-thunk)
              (and (procedure? failure-thunk)
                   (procedure-arity-includes? failure-thunk 0)))
    (raise-argument-error 'namespace-variable-value
                          "(or/c #f (procedure-arity-includes/c 0))"
                          failure-thunk))
  (check 'namespace-variable-value namespace? ns)
  ((let/ec escape
     (define-values (var-ns var-phase-level var-sym)
       (cond
        [use-mapping?
         (define id (datum->syntax #f sym))
         (define b (resolve+shift/extra-inspector (namespace-syntax-introduce id ns)
                                                  (namespace-phase ns)
                                                  ns))
         (define v (if b
                       (binding-lookup b empty-env null ns (namespace-phase ns) id)
                       variable))
         (unless (variable? v)
           (escape
            (or failure-thunk
                (lambda ()
                  (raise (exn:fail:syntax 
                          (format (string-append "namespace-variable-value: bound to syntax\n"
                                                 "  in: ~s")
                                  sym)
                          (current-continuation-marks)
                          null))))))
         (if (module-binding? b)
             (values (namespace->module-namespace ns
                                                  (module-binding-module b)
                                                  (phase- (namespace-phase ns)
                                                          (module-binding-phase b)))
                     (module-binding-phase b)
                     (module-binding-sym b))
             (values ns (namespace-phase ns) sym))]
        [else
         (values ns (namespace-phase ns) sym)]))
     (define val
       (namespace-get-variable var-ns var-phase-level var-sym
                               (lambda () (escape
                                      (or failure-thunk
                                          (raise exn:fail:contract:variable
                                                 (format (string-append
                                                          "namespace-variable-value: given name is not defined\n"
                                                          "  name: ~s"))
                                                 sym))))))
     (lambda () val))))

(define (namespace-set-variable-value! sym	 	 	 	 
                                       val
                                       [map? #f]
                                       [ns (current-namespace)])
  (check 'namespace-variable-value symbol? sym)
  (check 'namespace-variable-value namespace? ns)
  (namespace-set-variable! ns (namespace-phase ns) sym val)
  (when map?
    (define id (datum->syntax #f sym))
    (add-binding! (namespace-syntax-introduce id ns)
                  (make-module-binding (namespace-mpi ns)
                                       (namespace-phase ns)
                                       sym)
                  (namespace-phase ns))))

(define (namespace-undefine-variable! sym	 	 	 	 
                                      [ns (current-namespace)])
  (check 'namespace-variable-value symbol? sym)
  (check 'namespace-variable-value namespace? ns)
  (namespace-unset-variable! ns (namespace-phase ns) sym))

;; FIXME
(define (namespace-mapped-symbols [ns (current-namespace)])
  (check 'namespace-mapped-symbols namespace? ns)
  null)

(define (namespace-base-phase [ns (current-namespace)])
  (check 'namespace-base-phase namespace? ns)
  (namespace-phase ns))
