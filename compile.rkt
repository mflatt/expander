#lang racket/base
(require "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "namespace.rkt"
         "binding.rkt"
         "match.rkt"
         "core.rkt")

(provide compile
         expand-time-eval
         run-time-eval)

(define phase-shift-id (gensym 'phase))
(define ns-id (gensym 'namespace))

;; Convert an expanded syntax object to an expression that is represented
;; by a plain S-expression. The expression is compiled for a particular
;; phase, but if the expression is in a module, its phase can be shifted
;; at run time by the amount bound to `phase-shift-id`. Module bindings
;; are accessed through a namespace that is bound to `ns-id` at run time.
(define (compile s
                 [phase 0]      ; phase (top level) or phase-level (within a module)
                 [ns (current-namespace)] ; compile-time namespace
                 [self-name #f]) ; if non-#f, compiling the body of a module
  (let ([compile (lambda (s) (compile s phase ns self-name))])
    (cond
     [(pair? (syntax-e s))
      (define core-sym (core-form-sym s phase))
      (case core-sym
        [(#f)
         (error "not a core form:" s)]
        [(module)
         (define m (match-syntax s '(module name initial-require form ...)))
         (compile-module (syntax-e (m 'name))
                         (syntax-property s 'module-requires)
                         (syntax-property s 'module-provides)
                         (m 'form)
                         ns)]
        [(lambda)
         (define m (match-syntax s '(lambda formals body)))
         `(lambda ,@(compile-lambda (m 'formals) (m 'body) phase ns self-name))]
        [(case-lambda)
         (define m (match-syntax s '(case-lambda [formals body] ...)))
         `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                               [body (in-list (m 'body))])
                      (compile-lambda formals body phase ns self-name)))]
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
         (compile-let core-sym s phase ns self-name)]
        [(quote)
         (define m (match-syntax s '(quote datum)))
         `(quote ,(syntax->datum (m 'datum)))]
        [(quote-syntax)
         (define m (match-syntax s '(quote datum)))
         (define q `(quote ,(m 'datum)))
         (if self-name
             `(syntax-shift-phase-level ,q ,phase-shift-id)
             q)]
        [else
         (error "unrecognized core form:" core-sym)])]
     [(identifier? s)
      (define b (resolve s phase))
      (cond
       [(local-binding? b)
        (define sym (key->symbol (local-binding-key b)))
        (unless sym
          (error "missing a binding after expansion:" s))
        sym]
       [(module-binding? b)
        (define mod-name (module-binding-module b))
        (cond
         [(equal? mod-name '#%core)
          ;; Inline a core binding:
          (define m-ns (namespace->module-namespace ns mod-name phase))
          (or (namespace-get-variable m-ns (module-binding-phase b) (module-binding-sym b) #f)
              (error "internal error: bad #%core reference:"
                     phase (module-binding-sym b) (module-binding-phase b)))]
         [(equal? mod-name self-name)
          ;; Direct reference to a variable defined in the same module:
          (module-binding-sym b)]
         [else
          ;; Reference to a variable defined in another module:
          `(namespace-get-variable
            (namespace->module-namespace ,(if self-name
                                              ns-id
                                              ns)
                                         ',mod-name
                                         ,(let ([phase (phase- phase
                                                               (module-binding-phase b))])
                                            (if self-name
                                                `(+ ,phase-shift-id ,phase)
                                                phase)))
            ,(module-binding-phase b)
            ',(module-binding-sym b)
            (lambda () (error "undefined:"
                         ,phase
                         ',(module-binding-sym b)
                         ,(module-binding-phase b))))])]
       [else
        (error "not a reference to a local binding:" s)])]
     [else
      (error "bad syntax after expansion:" s)])))

(define (compile-lambda formals body phase ns self-name)
  (define gen-formals
    (let loop ([formals formals])
      (cond
       [(identifier? formals) (local->symbol formals phase)]
       [(syntax? formals) (loop (syntax-e formals))]
       [(pair? formals) (cons (loop (car formals))
                              (loop (cdr formals)))]
       [else null])))
  `(,gen-formals ,(compile body phase ns self-name)))

(define (compile-let core-sym s phase ns self-name)
  (define rec? (eq? core-sym 'letrec-values))
  (define m (match-syntax s '(let-values ([(id ...) rhs] ...) body)))
  (define sc (new-scope))
  (define idss (m 'id))
  (define symss (for/list ([ids (in-list idss)])
                  (for/list ([id (in-list ids)])
                    (local->symbol id phase))))
  `(,core-sym ,(for/list ([syms (in-list symss)]
                          [rhs (in-list (m 'rhs))])
                 `[,syms ,(compile rhs phase ns self-name)])
    ,(compile (m 'body) phase ns self-name)))

;; ----------------------------------------
                       
(define (compile-module name requires provides bodys ns)
  (define phase-level-id (gensym 'phase-level))
  
  (define self 'self)
  
  (define (add-body phase-to-body phase body)
    (hash-update phase-to-body phase (lambda (l) (cons body l)) null))
  
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
           (define syms (def-ids-to-syms (m 'id) phase))
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  phase
                  `(begin
                    (define-values ,syms ,(compile (m 'rhs) phase ns self))
                    ,@(for/list ([sym (in-list syms)])
                        `(namespace-set-variable! ,ns-id ,phase ',sym ,sym)))))]
          [(define-syntaxes)
           (define m (match-syntax (car bodys) '(define-syntaxes (id ...) rhs)))
           (define syms (def-ids-to-syms (m 'id) phase))
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  (add1 phase)
                  `(let-values ([,syms ,(compile (m 'rhs) (add1 phase) ns self)])
                    ,@(for/list ([sym (in-list syms)])
                        `(namespace-set-transformer! ,ns-id ,(sub1 phase) ',sym ,sym)))))]
          [(begin-for-syntax)
           (define m (match-syntax (car bodys) `(begin-for-syntax e ...)))
           (loop (cdr bodys)
                 phase
                 (loop (m 'e) (add1 phase) phase-to-body))]
          [(#%require #%provide)
           (loop (cdr bodys)
                 phase
                 phase-to-body)]
          [else
           (loop (cdr bodys)
                 phase
                 (add-body
                  phase-to-body
                  phase
                  (compile (car bodys) phase ns self)))])])))
  
  (define-values (min-phase max-phase)
    (for/fold ([min-phase 0] [max-phase 0]) ([phase (in-hash-keys phase-to-bodys)])
      (values (min min-phase phase)
              (max max-phase phase))))

  `(declare-module!
    (current-namespace)
    ',name
    (make-module
     ,requires
     ,provides
     ,min-phase
     ,max-phase
     (lambda (,ns-id ,phase-shift-id ,phase-level-id)
       (case ,phase-level-id
         ,@(for/list ([(phase bodys) (in-hash phase-to-bodys)])
             `[(,phase)
               ,@(reverse bodys)]))
       (void)))))

;; ----------------------------------------
         
(define (local->symbol id phase)
  (define b (resolve id phase))
  (unless (local-binding? b)
    (error "bad binding:" id))
  (key->symbol (local-binding-key b)))

(define (key->symbol key)
  ;; A local-binding key is already a symbol
  key)
 
(define (def-ids-to-syms ids phase)
  (for/list ([id (in-list ids)])
    (define b (resolve id phase))
    (unless (and (module-binding? b)
                 (eq? 'self (module-binding-module b))
                 (eqv? phase (module-binding-phase b)))
      (error "bad binding for module definition:" id))
    (module-binding-sym b)))

;; ----------------------------------------

(define expand-time-namespace (make-base-namespace))
(define (add-expand-time! sym val)
  (namespace-set-variable-value! sym val #t expand-time-namespace))

(add-expand-time! 'current-namespace current-namespace)
(add-expand-time! 'namespace->module-namespace namespace->module-namespace)
(add-expand-time! 'namespace-get-variable namespace-get-variable)
(add-expand-time! 'syntax-shift-phase-level syntax-shift-phase-level)

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

(define (expand-time-eval compiled)
  (eval compiled expand-time-namespace))

(define (run-time-eval compiled)
  (eval compiled run-time-namespace))
