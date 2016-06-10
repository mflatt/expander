#lang racket/base
(require "set.rkt"
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "namespace.rkt"
         "binding.rkt"
         "match.rkt"
         "core.rkt"
         "module-path.rkt"
         "built-in-symbol.rkt"
         "compile-context.rkt"
         "compile-header.rkt"
         "compile-impl-id.rkt"
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
(define (compile s cctx)
  (let ([compile (lambda (s) (compile s cctx))])
    (define phase (compile-context-phase cctx))
    (cond
     [(pair? (syntax-e s))
      (define core-sym (core-form-sym s phase))
      (case core-sym
        [(#f)
         (error "not a core form:" s)]
        [(module module*)
         (error "not a core expression form:" s)]
        [(lambda λ)
         (define m (match-syntax s '(lambda formals body)))
         (correlate s `(lambda ,@(compile-lambda (m 'formals) (m 'body) cctx)))]
        [(case-lambda)
         (define m (match-syntax s '(case-lambda [formals body] ...)))
         (correlate s `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                                            [body (in-list (m 'body))])
                                   (compile-lambda formals body cctx))))]
        [(#%app)
         (define m (match-syntax s '(#%app . rest)))
         (for/list ([s (in-list (correlated->list (m 'rest)))])
           (compile s))]
        [(if)
         (define m (match-syntax s '(if tst thn els)))
         (correlate s `(if
                        ,(compile (m 'tst))
                        ,(compile (m 'thn))
                        ,(compile (m 'els))))]
        [(with-continuation-mark)
         (define m (match-syntax s '(if key val body)))
         (correlate s `(with-continuation-mark
                        ,(compile (m 'key))
                        ,(compile (m 'val))
                        ,(compile (m 'body))))]
        [(begin begin0)
         (define m (match-syntax s '(begin e ...+)))
         (correlate s `(,core-sym ,@(for/list ([e (in-list (m 'e))])
                                      (compile e))))]
        [(set!)
         (define m (match-syntax s '(set! id rhs)))
         (correlate s `(,@(compile-identifier (m 'id) cctx
                                              #:set-to (compile (m 'rhs)))))]
        [(let-values letrec-values)
         (compile-let core-sym s cctx)]
        [(#%expression)
         (define m (match-syntax s '(#%expression e)))
         (compile (m 'e))]
        [(quote)
         (define m (match-syntax s '(quote datum)))
         (correlate s `(quote ,(syntax->datum (m 'datum))))]
        [(quote-syntax)
         (define m (match-syntax s '(quote datum . _)))
         (compile-quote-syntax (m 'datum) phase cctx)]
        [(#%variable-reference)
         (define id-m (try-match-syntax s '(#%variable-reference id)))
         (define top-m (and (not id-m)
                            (try-match-syntax s '(#%variable-reference (#%top . id)))))
         (define id (or (and id-m (id-m 'id))
                        (and top-m (top-m 'id))))
         (correlate s 
                    (if id
                        `(#%variable-reference ,(compile-identifier id cctx))
                        `(#%variable-reference)))]
        [(#%top)
         (when (compile-context-module-self cctx)
           (error "found `#%top` in a module body:" s))
         (define m (match-syntax s '(#%top . id)))
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
  `(,gen-formals ,(compile body cctx)))

(define (compile-let core-sym s cctx)
  (define rec? (eq? core-sym 'letrec-values))
  (define m (match-syntax s '(let-values ([(id ...) rhs] ...) body)))
  (define phase (compile-context-phase cctx))
  (define idss (m 'id))
  (define symss (for/list ([ids (in-list idss)])
                  (for/list ([id (in-list ids)])
                    (local-id->symbol id phase))))
  (correlate s
             `(,core-sym ,(for/list ([syms (in-list symss)]
                                     [rhs (in-list (m 'rhs))])
                            `[,syms ,(compile rhs cctx)])
               ,(compile (m 'body) cctx))))

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
        ;; Inline a core binding:
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
                                         (module-binding-sym b))])]
     [else
      (error "not a reference to a module or local binding:" s)]))
  (correlate s (if rhs
                   `(set! ,sym ,rhs)
                   sym)))

;; Pick a symbol to represent a local binding, given the identifier
(define (local-id->symbol id phase)
  (define b (resolve id phase))
  (unless (local-binding? b)
    (error "bad binding:" id phase))
  (local-key->symbol (local-binding-key b)))


(define (compile-quote-syntax q phase cctx)
  (define pos (add-syntax-literal! (compile-context-header cctx) q))
  (cond
   [(compile-context-lazy-syntax-literals? cctx)
    `(let-values ([(stx) (vector-ref ,syntax-literals-id ,pos)])
      (if stx
          stx
          (,get-syntax-literal!-id ',pos)))]
   [else
    `(vector-ref (vector-ref ,syntax-literalss-id ',phase) ',pos)]))
