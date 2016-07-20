#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/to-list.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/property.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../syntax/binding.rkt"
         "../syntax/match.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "built-in-symbol.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "self-quoting.rkt"
         "../host/correlate.rkt"
         "correlate.rkt")

(provide compile
         compile-quote-syntax)

;; Convert an expanded syntax object to an expression that is
;; represented by a plain S-expression plus source location info (so,
;; still represented as a syntax object). The expression is compiled
;; for a particular phase, but if the expression is in a module, its
;; phase can be shifted at run time by the amount bound to
;; `phase-shift-id`. Module bindings are accessed through a namespace
;; that is bound to `ns-id` at run time.
;; The `result-used?` hint lets us drop `quote-syntax` forms that will
;; not be used in the result, so we can avoid serializing them; a value
;; of `#f` for `result-used?` means that the expression can be replaced
;; by a boolean-equivalent value if it has no side effect.
(define (compile in-s cctx [name #f] [result-used? #t])
  (let ([compile (lambda (s name result-used?) (compile s cctx name result-used?))])
    (define s (syntax-disarm in-s))
    (cond
     [(pair? (syntax-e s))
      (define phase (compile-context-phase cctx))
      (define core-sym (core-form-sym s phase))
      (case core-sym
        [(#f)
         (error "internal error; not a core form:" s "at phase:" phase)]
        [(module module*)
         (error "not a core expression form:" s)]
        [(lambda)
         (cond
          [result-used?
           (define-match m s '(lambda formals body))
           (add-lambda-properties
            (correlate* s `(lambda ,@(compile-lambda (m 'formals) (m 'body) cctx)))
            name
            s)]
          [else (correlate* s `(quote ,(syntax->datum s)))])]
        [(case-lambda)
         (cond
          [result-used?
           (define-match m s '(case-lambda [formals body] ...))
           (add-lambda-properties
            (correlate* s `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                                                [body (in-list (m 'body))])
                                       (compile-lambda formals body cctx))))
            name
            s)]
          [else (correlate* s `(quote ,(syntax->datum s)))])]
        [(#%app)
         (define-match m s '(#%app . rest))
         (define es (let ([es (m 'rest)])
                      (if (syntax? es)
                          (syntax->list (syntax-disarm es))
                          es)))
         (for/list ([s (in-list es)])
           (compile s #f #t))]
        [(if)
         (define-match m s '(if tst thn els))
         (correlate* s `(if
                         ,(compile (m 'tst) #f #f)
                         ,(compile (m 'thn) name result-used?)
                         ,(compile (m 'els) name result-used?)))]
        [(with-continuation-mark)
         (define-match m s '(if key val body))
         (correlate* s `(with-continuation-mark
                         ,(compile (m 'key) #f #t)
                         ,(compile (m 'val) #f #t)
                         ,(compile (m 'body) name result-used?)))]
        [(begin begin0)
         (define-match m s '(begin e ...+))
         (define used-pos (case core-sym
                            [(begin0) 0]
                            [else (sub1 (length (m 'e)))]))
         (correlate* s `(,core-sym ,@(for/list ([e (in-list (m 'e))]
                                                [i (in-naturals)])
                                       (define used? (= i used-pos))
                                       (compile e (and used? name) used?))))]
        [(set!)
         (define-match m s '(set! id rhs))
         (correlate* s `(,@(compile-identifier (m 'id) cctx
                                               #:set-to (compile (m 'rhs) (m 'id) #t))))]
        [(let-values)
         (compile-let core-sym s cctx name #:rec? #f result-used?)]
        [(letrec-values)
         (compile-let core-sym s cctx name #:rec? #f result-used?)]
        [(#%expression)
         (define-match m s '(#%expression e))
         (compile (m 'e) name result-used?)]
        [(quote)
         (define-match m s '(quote datum))
         (define datum (syntax->datum (m 'datum)))
         (cond
          [(self-quoting-in-linklet? datum)
           (correlate* s datum)]
          [else
           (correlate* s `(quote ,datum))])]
        [(quote-syntax)
         (define-match m s '(quote-syntax datum . _))
         (if result-used?
             (compile-quote-syntax (m 'datum) phase cctx)
             (correlate* s `(quote ,(syntax->datum (m 'datum)))))]
        [(#%variable-reference)
         (define-match id-m s #:try '(#%variable-reference id))
         (define-match top-m s #:unless (id-m) #:try '(#%variable-reference (#%top . id)))
         (define id (or (and (id-m) (id-m 'id))
                        (and (top-m) (top-m 'id))))
         (correlate* s 
                     (if id
                         `(#%variable-reference ,(compile-identifier id cctx))
                         `(#%variable-reference)))]
        [(#%top)
         (when (compile-context-module-self cctx)
           (error "found `#%top` in a module body:" s))
         (define-match m s '(#%top . id))
         (compile-identifier (m 'id) cctx #:top? #t)]
        [else
         (error "unrecognized core form:" core-sym)])]
     [(identifier? s)
      (compile-identifier s cctx)]
     [else
      (error "bad syntax after expansion:" s)])))

(define (compile-lambda formals body cctx)
  (define phase (compile-context-phase cctx))
  (define gen-formals
    (let loop ([formals formals])
      (cond
       [(identifier? formals) (local-id->symbol formals phase)]
       [(syntax? formals) (loop (syntax-e formals))]
       [(pair? formals) (cons (loop (car formals))
                              (loop (cdr formals)))]
       [else null])))
  `(,gen-formals ,(compile body cctx #f)))

(define (add-lambda-properties s inferred-name orig-s)
  (define name (or inferred-name
                   (syntax-property orig-s inferred-name)))
  (define named-s (if name
                      (correlated-property s
                                           'inferred-name
                                           (if (syntax? name) (syntax-e name) name))
                      s))
  (define as-method (syntax-property orig-s 'method-arity-error))
  (if as-method
      (correlated-property named-s 'method-arity-error as-method)
      named-s))

(define (compile-let core-sym s cctx name #:rec? rec? result-used?)
  (define rec? (eq? core-sym 'letrec-values))
  (define-match m s '(let-values ([(id ...) rhs] ...) body))
  (define phase (compile-context-phase cctx))
  (define idss (m 'id))
  (define symss (for/list ([ids (in-list idss)])
                  (for/list ([id (in-list ids)])
                    (define sym (local-id->symbol id phase))
                    (if rec?
                        (add-undefined-error-name-property sym id)
                        sym))))
  (correlate* s
              `(,core-sym ,(for/list ([syms (in-list symss)]
                                      [ids (in-list idss)]
                                      [rhs (in-list (m 'rhs))])
                             `[,syms ,(compile rhs
                                               cctx
                                               (and (= 1 (length ids)) (car ids)))])
                ,(compile (m 'body) cctx name result-used?))))

(define (add-undefined-error-name-property sym orig-id)
  (define id (correlate* orig-id sym))
  (correlated-property id 'undefined-error-name
                       (or (syntax-property orig-id 'undefined-error-name)
                           (syntax-e orig-id))))

(define (compile-identifier s cctx #:set-to [rhs #f] #:top? [top? #f])
  (define phase (compile-context-phase cctx))
  (define normal-b (resolve+shift s phase))
  (define b
    (or normal-b
        ;; Assume a variable reference
        (make-module-binding (compile-context-self cctx)
                             phase
                             (syntax-e s))))
  (define sym
    (cond
     [(local-binding? b)
      (define sym (local-key->symbol (local-binding-key b)))
      (unless sym
        (error "missing a binding after expansion:" s))
      sym]
     [(module-binding? b)
      (define mpi (if top?
                      (compile-context-self cctx)
                      (module-binding-module b)))
      (define mod-name (module-path-index-resolve mpi))
      (define ns (compile-context-namespace cctx))
      (define mod (namespace->module ns mod-name))
      (cond
       [(and mod (module-primitive? mod))
        ;; Direct reference to a runtime primitive:
        (unless (zero? (module-binding-phase b))
          (error "internal error: non-zero phase for a primitive"))
        (when rhs
          (error "internal error: cannot assign to a primitive:" s))
        (namespace-module-instantiate! ns mpi 0)
        (define m-ns (namespace->module-namespace ns mod-name 0))
        ;; Expect each primitive to be bound:
        (module-binding-sym b)]
       [(eq? mpi (compile-context-module-self cctx))
        ;; Direct reference to a variable defined in the same module:
        (define header (compile-context-header cctx))
        (hash-ref (header-binding-sym-to-define-sym header)
                  (module-binding-sym b))]
       [else
        ;; Reference to a variable defined in another module or in an
        ;; environment (such as the top level) other than a module
        ;; context; register as a linklet import
        (register-required-variable-use! (compile-context-header cctx)
                                         mpi
                                         (module-binding-phase b)
                                         (module-binding-sym b)
                                         (or (module-binding-extra-inspector b)
                                             (syntax-inspector s)))])]
     [else
      (error "not a reference to a module or local binding:" s)]))
  (correlate* s (if rhs
                    `(set! ,sym ,rhs)
                    sym)))

;; Pick a symbol to represent a local binding, given the identifier
(define (local-id->symbol id phase)
  (define b (resolve id phase))
  (unless (local-binding? b)
    (error "bad binding:" id phase b))
  (local-key->symbol (local-binding-key b)))


(define (compile-quote-syntax q phase cctx)
  (define pos (add-syntax-literal! (compile-context-header cctx) q))
  (cond
   [(compile-context-lazy-syntax-literals? cctx)
    (generate-lazy-syntax-literal-lookup phase pos)]
   [else
    (generate-eager-syntax-literal-lookup phase pos)]))
