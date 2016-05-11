#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "namespace.rkt"
         "binding.rkt"
         "match.rkt"
         "core.rkt"
         "require+provide.rkt"
         "module-path.rkt"
         "expand-require.rkt")

(provide make-compile-context
         compile
         compile-module
         expand-time-eval
         run-time-eval)

(struct compile-context (namespace   ; compile-time namespace
                         phase       ; phase (top level) or phase level (within a module)
                         self        ; if non-#f module path index, compiling the body of a module
                         root-module-name ; set to a symbol if `self` is non-#f
                         allow-unbound-as-variable?))

(define (make-compile-context #:namespace [namespace (current-namespace)]
                              #:phase [phase 0]
                              #:self [self #f]
                              #:root-module-name [root-module-name #f]
                              #:allow-unbound-as-variable? [allow-unbound-as-variable? #f])
  (when (and self (not root-module-name))
    (error "internal error: self provided without root"))
  (compile-context namespace 
                   phase
                   self
                   root-module-name
                   allow-unbound-as-variable?))

(define phase-shift-id (gensym 'phase))
(define ns-id (gensym 'namespace))
(define self-id (gensym 'namespace))

;; ----------------------------------------

;; Convert an expanded syntax object to an expression that is represented
;; by a plain S-expression. The expression is compiled for a particular
;; phase, but if the expression is in a module, its phase can be shifted
;; at run time by the amount bound to `phase-shift-id`. Module bindings
;; are accessed through a namespace that is bound to `ns-id` at run time.
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
         (compile-module s cctx)]
        [(#%require)
         (define m (match-syntax s '(#%require req ...)))
         ;; Running the compiled code will trigger expander work ---
         ;; which is strange, and that reflects how a top-level
         ;; `#%require` is strange
         `(,(lambda ()
              (define ns (compile-context-namespace cctx))
              (parse-and-perform-requires! #:run? #t (m 'req) #f ns phase 
                                           (make-requires+provides #f))))]
        [(lambda)
         (define m (match-syntax s '(lambda formals body)))
         `(lambda ,@(compile-lambda (m 'formals) (m 'body) cctx))]
        [(case-lambda)
         (define m (match-syntax s '(case-lambda [formals body] ...)))
         `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                               [body (in-list (m 'body))])
                      (compile-lambda formals body cctx)))]
        [(#%app)
         (define m (match-syntax s '(#%app . rest)))
         (for/list ([s (in-list (m 'rest))])
           (compile s))]
        [(if)
         (define m (match-syntax s '(if tst thn els)))
         `(if
           ,(compile (m 'tst))
           ,(compile (m 'thn))
           ,(compile (m 'els)))]
        [(with-continuation-mark)
         (define m (match-syntax s '(if key val body)))
         `(with-continuation-mark
           ,(compile (m 'key))
           ,(compile (m 'val))
           ,(compile (m 'body)))]
        [(begin begin0)
         (define m (match-syntax s '(begin e ...+)))
         `(,core-sym ,@(for/list ([e (in-list (m 'e))])
                         (compile e)))]
        [(set!)
         (define m (match-syntax s '(set! id rhs)))
         `(set!
           ,(compile (m 'id))
           ,(compile (m 'rhs)))]
        [(let-values letrec-values)
         (compile-let core-sym s cctx)]
        [(#%expression)
         (define m (match-syntax s '(#%expression e)))
         (compile (m 'e))]
        [(quote)
         (define m (match-syntax s '(quote datum)))
         `(quote ,(syntax->datum (m 'datum)))]
        [(quote-syntax)
         (define m (match-syntax s '(quote datum)))
         (define q `(quote ,(m 'datum)))
         (if (compile-context-self cctx)
             `(syntax-module-path-index-shift
               (syntax-shift-phase-level ,q ,phase-shift-id)
               ',(compile-context-self cctx)
               ,self-id)
             q)]
        [else
         (error "unrecognized core form:" core-sym)])]
     [(identifier? s)
      (define b (resolve+shift s phase))
      (cond
       [(local-binding? b)
        (define sym (key->symbol (local-binding-key b)))
        (unless sym
          (error "missing a binding after expansion:" s))
        sym]
       [(module-binding? b)
        (define mpi (module-binding-module b))
        (define mod-name (module-path-index-resolve mpi))
        (cond
         [(equal? mod-name core-module-name)
          ;; Inline a core binding:
          (define ns (compile-context-namespace cctx))
          (define m-ns (namespace->module-namespace ns mod-name 0))
          (namespace-module-instantiate! m-ns core-module-name (module-binding-phase b))
          (or (namespace-get-variable m-ns (module-binding-phase b) (module-binding-sym b) #f)
              (error "internal error: bad #%core reference:"
                     phase (module-binding-sym b) (module-binding-phase b)))]
         [(eq? mpi (compile-context-self cctx))
          ;; Direct reference to a variable defined in the same module:
          (module-binding-sym b)]
         [else
          ;; Reference to a variable defined in another module:
          (define in-mod? (compile-context-self cctx))
          `(namespace-get-variable
            (namespace->module-namespace #:complain-on-failure? #t
                                         ,(if in-mod?
                                              ns-id
                                              (compile-context-namespace cctx))
                                         ,(if in-mod?
                                              `(module-path-index-resolve
                                                (module-path-index-shift
                                                 ',(module-binding-module b)
                                                 ',(compile-context-self cctx)
                                                 ,self-id))
                                              `',(module-path-index-resolve
                                                  (module-binding-module b)))
                                         ,(let ([phase (phase- phase
                                                               (module-binding-phase b))])
                                            (if in-mod?
                                                `(+ ,phase-shift-id ,phase)
                                                phase)))
            ,(module-binding-phase b)
            ',(module-binding-sym b)
            (lambda () (error "undefined:"
                         ,phase
                         ',(module-binding-sym b)
                         ,(module-binding-phase b))))])]
       [(compile-context-allow-unbound-as-variable? cctx)
        (syntax-e s)]
       [else
        (error "not a reference to a module or local binding:" s)])]
     [else
      (error "bad syntax after expansion:" s)])))

(define (compile-lambda formals body cctx)
  (define phase (compile-context-phase cctx))
  (define gen-formals
    (let loop ([formals formals])
      (cond
       [(identifier? formals) (local->symbol formals phase)]
       [(syntax? formals) (loop (syntax-e formals))]
       [(pair? formals) (cons (loop (car formals))
                              (loop (cdr formals)))]
       [else null])))
  `(,gen-formals ,(compile body cctx)))

(define (compile-let core-sym s cctx)
  (define rec? (eq? core-sym 'letrec-values))
  (define m (match-syntax s '(let-values ([(id ...) rhs] ...) body)))
  (define phase (compile-context-phase cctx))
  (define sc (new-scope))
  (define idss (m 'id))
  (define symss (for/list ([ids (in-list idss)])
                  (for/list ([id (in-list ids)])
                    (local->symbol id phase))))
  `(,core-sym ,(for/list ([syms (in-list symss)]
                          [rhs (in-list (m 'rhs))])
                 `[,syms ,(compile rhs cctx)])
    ,(compile (m 'body) cctx)))

;; ----------------------------------------
                       
(define (compile-module s cctx
                        #:self [given-self #f]
                        #:as-submodule? [as-submodule? #f])
  (define m (match-syntax s '(module name initial-require
                              (#%module-begin body ...))))
  (define enclosing-self (compile-context-self cctx))
  (define self (or given-self
                   (make-generic-self-module-path-index
                    (make-self-module-path-index
                     (syntax-e (m 'name))
                     enclosing-self))))
  (define root-module-name (or (compile-context-root-module-name cctx)
                               (syntax-e (m 'name))))
  (define requires (syntax-property s 'module-requires))
  (define provides (syntax-property s 'module-provides))
  (define bodys (m 'body))

  (define phase-level-id (gensym 'phase-level))
  
  (define (add-body phase-to-body phase body)
    (hash-update phase-to-body phase (lambda (l) (cons body l)) null))
  
  (define body-cctx (struct-copy compile-context cctx
                                 [self self]
                                 [root-module-name root-module-name]))
  
  (define phase-to-bodys
    (let loop ([bodys bodys]
               [phase 0]
               [phase-to-body #hasheqv()])
      (cond
       [(null? bodys) phase-to-body]
       [else
        (case (core-form-sym (car bodys) phase)
          [(define-values)
           (define m (match-syntax (car bodys) '(define-values (id ...) rhs)))
           (define syms (def-ids-to-syms (m 'id) phase self))
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  phase
                  `(begin
                    (define-values ,syms ,(compile (m 'rhs)
                                                   (struct-copy compile-context body-cctx
                                                                [phase phase])))
                    ,@(for/list ([sym (in-list syms)])
                        `(namespace-set-variable! ,ns-id ,phase ',sym ,sym)))))]
          [(define-syntaxes)
           (define m (match-syntax (car bodys) '(define-syntaxes (id ...) rhs)))
           (define syms (def-ids-to-syms (m 'id) phase self))
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  (add1 phase)
                  `(let-values ([,syms ,(compile (m 'rhs)
                                                 (struct-copy compile-context body-cctx
                                                              [phase (add1 phase)]))])
                    ,@(for/list ([sym (in-list syms)])
                        `(namespace-set-transformer! ,ns-id ,phase ',sym ,sym)))))]
          [(begin-for-syntax)
           (define m (match-syntax (car bodys) `(begin-for-syntax e ...)))
           (loop (cdr bodys)
                 phase
                 (loop (m 'e) (add1 phase) phase-to-body))]
          [(#%require #%provide)
           (loop (cdr bodys)
                 phase
                 phase-to-body)]
          [(module module*)
           ;; Submodules are handled separately below
           (loop (cdr bodys)
                 phase
                 phase-to-body)]
          [else
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  phase
                  (compile (car bodys)
                           (struct-copy compile-context body-cctx
                                        [phase phase]))))])])))
  
  (define (compile-submodules form-name)
    (cond
     [as-submodule?
      null]
     [else
      (let loop ([bodys bodys]
                 [phase 0])
        (cond
         [(null? bodys) null]
         [else
          (define f (core-form-sym (car bodys) phase))
          (cond
           [(eq? f form-name)
            (define s-shifted
              (cond
               [(try-match-syntax (car bodys) '(module* name #f . _))
                (syntax-shift-phase-level (car bodys) (phase- 0 phase))]
               [else (car bodys)]))
            (cons (compile-module s-shifted body-cctx)
                  (loop (cdr bodys) phase))]
           [(eq? f 'begin-for-syntax)
            (define m (match-syntax (car bodys) `(begin-for-syntax e ...)))
            (append (loop (m 'e) (add1 phase))
                    (loop (cdr bodys) phase))]
          [else
           (loop (cdr bodys) phase)])]))]))

  (define pre-submodules (compile-submodules 'module))
  (define post-submodules (compile-submodules 'module*))

  (define-values (min-phase max-phase)
    (for/fold ([min-phase 0] [max-phase 0]) ([phase (in-hash-keys phase-to-bodys)])
      (values (min min-phase phase)
              (max max-phase phase))))

  `(begin
    ,@pre-submodules
    (declare-module!
     #:as-submodule? ,as-submodule?
     (current-namespace)
     (module-shift-for-declare
      (make-module
       ',self
       ,requires
       ,provides
       ,min-phase
       ,max-phase
       (lambda (,ns-id ,phase-shift-id ,phase-level-id ,self-id)
         (case ,phase-level-id
           ,@(for/list ([(phase bodys) (in-hash phase-to-bodys)])
               `[(,phase)
                 ,@(reverse bodys)]))
         (void)))
      (substitute-module-declare-name ',root-module-name
                                      ',(module-path-index-resolve self))))
    ,@post-submodules))

;; ----------------------------------------
         
(define (local->symbol id phase)
  (define b (resolve id phase))
  (unless (local-binding? b)
    (error "bad binding:" id))
  (key->symbol (local-binding-key b)))

(define (key->symbol key)
  ;; A local-binding key is already a symbol
  key)
 
(define (def-ids-to-syms ids phase self)
  (for/list ([id (in-list ids)])
    (define b (resolve+shift id phase))
    (unless (and (module-binding? b)
                 (eq? self (module-binding-module b))
                 (eqv? phase (module-binding-phase b)))
      (error "bad binding for module definition:" id
             self "vs." (module-binding-module b)))
    (module-binding-sym b)))

;; ----------------------------------------

(define expand-time-namespace (make-base-namespace))
(define (add-expand-time! sym val)
  (namespace-set-variable-value! sym val #t expand-time-namespace))

(add-expand-time! 'current-namespace current-namespace)
(add-expand-time! 'namespace->module-namespace namespace->module-namespace)
(add-expand-time! 'namespace-get-variable namespace-get-variable)
(add-expand-time! 'syntax-shift-phase-level syntax-shift-phase-level)
(add-expand-time! 'module-path-index-shift module-path-index-shift)
(add-expand-time! 'module-path-index-resolve module-path-index-resolve)

(define run-time-namespace (make-base-namespace))
(define (add-run-time! sym val)
  (namespace-set-variable-value! sym val #t run-time-namespace))

(add-run-time! 'make-module make-module)
(add-run-time! 'declare-module! declare-module!)
(add-run-time! 'current-namespace current-namespace)
(add-run-time! 'namespace-set-variable! namespace-set-variable!)
(add-run-time! 'namespace-set-transformer! namespace-set-transformer!)
(add-run-time! 'namespace-get-variable namespace-get-variable)
(add-run-time! 'namespace->module-namespace namespace->module-namespace)
(add-run-time! 'syntax-shift-phase-level syntax-shift-phase-level)
(add-run-time! 'module-path-index-shift module-path-index-shift)
(add-run-time! 'module-path-index-resolve module-path-index-resolve)
(add-run-time! 'module-shift-for-declare module-shift-for-declare)
(add-run-time! 'substitute-module-declare-name substitute-module-declare-name)
(add-run-time! 'syntax-module-path-index-shift syntax-module-path-index-shift)

(define (expand-time-eval compiled)
  (eval compiled expand-time-namespace))

(define (run-time-eval compiled)
  (eval compiled run-time-namespace))
