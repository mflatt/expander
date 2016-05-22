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
         "expand-require.rkt"
         "variable-reference.rkt"
         "serialize.rkt"
         (only-in racket/base
                  [current-namespace base:current-namespace]
                  [compile base:compile]))

(provide make-compile-context
         compile-top
         compile-module
         expand-time-eval
         run-time-eval
         compile-install-primitives!)

(struct compile-context (namespace   ; compile-time namespace
                         phase       ; phase (top level) or phase level (within a module)
                         self        ; if non-#f module path index, compiling the body of a module
                         compile-time-for-self ; for allowing forward references across `begin-for-syntax`
                         root-module-name ; set to a symbol if `self` is non-#f
                         top-init    ; accumulates top-level initialization
                         def-syms))  ; selects names that don't collide with primitives: sym -> sym 

(struct top-init (module-path-indexes ; module-path-index -> pos
                  variable-uses      ; variable-use -> sym
                  syntax-literals    ; box of list syntax-literal
                  [num-syntax-literals #:mutable]))

(struct variable-use (module phase sym)
        #:property prop:equal+hash
        ;; Hach/compare with `eq?` on module part:
        (list (lambda (a b eql?)
                (and (eq? (variable-use-module a)
                          (variable-use-module b))
                     (eqv? (variable-use-phase a)
                           (variable-use-phase b))
                     (eq? (variable-use-sym a)
                          (variable-use-sym b))))
              (lambda (a hash-code)
                (and (+ (eq-hash-code (variable-use-module a))
                        (eqv-hash-code (variable-use-phase a))
                        (eq-hash-code (variable-use-sym a)))))
              (lambda (a hash-code)
                (and (eq-hash-code (variable-use-sym a))))))

(define (make-compile-context #:namespace [namespace (current-namespace)]
                              #:phase [phase (namespace-phase namespace)]
                              #:self [self #f]
                              #:compile-time-for-self [compile-time-for-self #f]
                              #:root-module-name [root-module-name #f])
  (when (and self (not root-module-name))
    (error "internal error: self provided without root"))
  (compile-context namespace 
                   phase
                   self
                   compile-time-for-self
                   root-module-name
                   #f
                   (make-hasheq)))

(define phase-shift-id (gensym 'phase))
(define ns-id (gensym 'namespace))
(define self-id (gensym 'namespace))
(define syntax-literals-id (gensym 'syntax-literals))
(define unshifted-syntax-literals-id (gensym 'unshifted-syntax-literals))
(define get-syntax-literal!-id (gensym 'get-syntax-literal!))
(define bulk-binding-registry-id (gensym 'bulk-binding-registry))
(define deserialized-syntax-id (gensym 'deserialized-syntax))

;; ----------------------------------------

(define (compile-top s cctx)
  (define top-init (make-top-init #f))
  (define compiled
    (compile s (struct-copy compile-context cctx
                            [top-init top-init])))
  (define s-expr
    `(begin
      ,@(generate-top-init top-init
                           (compile-context-phase cctx)
                           cctx)
      ,compiled))
  (parameterize ([base:current-namespace run-time-namespace])
    (base:compile s-expr)))
   
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
        [(lambda λ)
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
         `(,@(compile-identifier (m 'id) cctx
                                 #:set-to (compile (m 'rhs))))]
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
         (define q (m 'datum))
         (if (compile-context-self cctx)
             (let ([pos (add-syntax-literal! (compile-context-top-init cctx) q)])
               `(let ([stx (vector-ref ,syntax-literals-id ,pos)])
                 (if stx
                     (,get-syntax-literal!-id ,pos)
                     stx)))
             `(quote ,q))]
        [(#%variable-reference)
         (define id-m (try-match-syntax s '(#%variable-reference id)))
         (define top-m (and (not id-m)
                            (try-match-syntax s '(#%variable-reference (#%top . id)))))
         (define id (or (and id-m (id-m 'id))
                        (and top-m (top-m 'id))))
         (define binding (and id (resolve+shift id phase)))
         (define in-mod? (compile-context-self cctx))
         `(variable-reference 
           ,(if in-mod?
                self-id
                `',(compile-context-self cctx))
           ,(if in-mod?
                ns-id
                (compile-context-namespace cctx))
           ,(if in-mod?
                `(+ ,phase-shift-id ,phase)
                phase)
           ,(if in-mod?
                phase-shift-id
                0)
           ;; FIXME: compute mutations
           #f)]
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
  (define idss (m 'id))
  (define symss (for/list ([ids (in-list idss)])
                  (for/list ([id (in-list ids)])
                    (local->symbol id phase))))
  `(,core-sym ,(for/list ([syms (in-list symss)]
                          [rhs (in-list (m 'rhs))])
                 `[,syms ,(compile rhs cctx)])
    ,(compile (m 'body) cctx)))

(define (compile-identifier s cctx #:set-to [rhs #f])
  (define (direct sym)
    (if rhs
        `(set! ,sym ,rhs)
        sym))
  (define (indirect sym)
    (if rhs
        `(set-box! ,sym ,rhs)
        `(unbox ,sym)))
  (define phase (compile-context-phase cctx))
  (define normal-b (resolve+shift s phase))
  (define b
    (cond
     [(and (not normal-b)
           (compile-context-compile-time-for-self cctx))
      ;; Assume a forward reference
      (make-module-binding (compile-context-compile-time-for-self cctx)
                           phase
                           (syntax-e s))]
     [else normal-b]))
  (cond
   [(local-binding? b)
    (define sym (key->symbol (local-binding-key b)))
    (unless sym
      (error "missing a binding after expansion:" s))
    (direct sym)]
   [(module-binding? b)
    (define mpi (module-binding-module b))
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
      (namespace-module-instantiate! ns mod-name 0)
      (define m-ns (namespace->module-namespace ns mod-name 0))
      ;; Expect each primitive to be bound:
      (module-binding-sym b)]
     [(eq? mpi (compile-context-self cctx))
      ;; Direct reference to a variable defined in the same module:
      (direct (convert-def-sym (module-binding-sym b)
                               (compile-context-def-syms cctx)))]
     [else
      ;; Reference to a variable defined in another module; we'll look up
      ;; the variable's box once at the top
      (define key (variable-use (module-binding-module b)
                                (module-binding-phase b)
                                (module-binding-sym b)))
      (define variable-uses (top-init-variable-uses
                             (compile-context-top-init cctx)))
      (define sym (or (hash-ref variable-uses key #f)
                      (let ([sym (gensym (format "~a+" (variable-use-sym key)))])
                        (hash-set! variable-uses key sym)
                        sym)))
      (cond
       [(eq? mpi (compile-context-compile-time-for-self cctx))
        ;; To allow forward references, we need an use-before-definition check
        (define check `(check-defined (unbox ,sym) ',(module-binding-sym b)))
        (if rhs
            `(begin
              ,check
              ,(indirect sym))
            check)]
       [else
        (indirect sym)])])]
   [else
    (error "not a reference to a module or local binding:" s)]))

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
  
  (define phase-to-body (make-hasheqv)) ; phase -> list of syntax
  (define (add-body! phase body)
    (hash-update! phase-to-body phase (lambda (l) (cons body l)) null))
  
  (define mpis (make-module-path-index-table))
  
  (define phase-to-top-init (make-hasheqv)) ; phase -> top-init
  (define (find-or-create-top-init! phase)
    (or (hash-ref phase-to-top-init phase #f)
        (let ([top-init (make-top-init mpis)])
          (hash-set! phase-to-top-init phase top-init)
          top-init)))
  
  (define cross-phase-persistent? #f)
  
  (define def-syms (make-hasheq))
  
  (define body-cctx (struct-copy compile-context cctx
                                 [self self]
                                 [root-module-name root-module-name]
                                 [def-syms def-syms]))
  
  (let loop! ([bodys bodys] [phase 0])
    (for ([body (in-list bodys)])
      (case (core-form-sym body phase)
        [(define-values)
         (define m (match-syntax body '(define-values (id ...) rhs)))
         (define syms (def-ids-to-syms (m 'id) phase self))
         (define dsyms (convert-def-syms syms def-syms))
         (add-body!
          phase
          `(begin
            (define-values ,dsyms ,(compile (m 'rhs)
                                            (struct-copy compile-context body-cctx
                                                         [phase phase]
                                                         [top-init (find-or-create-top-init! phase)])))
            ,@(for/list ([sym (in-list syms)]
                         [dsym (in-list dsyms)])
                `(namespace-set-variable! ,ns-id ,phase ',sym ,dsym))))]
        [(define-syntaxes)
         (define m (match-syntax body '(define-syntaxes (id ...) rhs)))
         (define syms (def-ids-to-syms (m 'id) phase self))
         (define dsyms (convert-def-syms syms def-syms))
         (add-body!
          (add1 phase)
          `(let-values ([,dsyms ,(compile (m 'rhs)
                                          (struct-copy compile-context body-cctx
                                                       [phase (add1 phase)]
                                                       [top-init (find-or-create-top-init! (add1 phase))]))])
            ,@(for/list ([sym (in-list syms)]
                         [dsym (in-list dsyms)])
                `(namespace-set-transformer! ,ns-id ,phase ',sym ,dsym))))]
        [(begin-for-syntax)
         (define m (match-syntax body `(begin-for-syntax e ...)))
         (loop! (m 'e) (add1 phase))]
        [(#%require #%provide)
         ;; Nothing to do at run time
         (void)]
        [(#%declare)
         (define m (match-syntax body '(#%declare kw ...)))
         (for ([kw (in-list (m 'kw))])
           (when (eq? (syntax-e kw) '#:cross-phase-persistent)
             (set! cross-phase-persistent? #t)))]
        [(module module*)
         ;; Submodules are handled separately below
         (void)]
        [else
         (add-body!
          phase
          (compile body
                   (struct-copy compile-context body-cctx
                                [phase phase]
                                [top-init (find-or-create-top-init! phase)])))])))
  
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
    (for/fold ([min-phase 0] [max-phase 0]) ([phase (in-hash-keys phase-to-body)])
      (values (min min-phase phase)
              (max max-phase phase))))
  
  (define module-declaration
    `(make-module
      #:cross-phase-persistent? ,cross-phase-persistent?
      ,(add-module-path-index! mpis self)
      ,(generate-deserialize requires mpis)
      ,(generate-deserialize provides mpis)
      ,min-phase
      ,max-phase
      (lambda (,ns-id ,phase-shift-id ,phase-level-id ,self-id ,bulk-binding-registry-id)
        (case ,phase-level-id
          ,@(for/list ([(phase bodys) (in-hash phase-to-body)])
              `[(,phase)
                ,@(generate-top-init (hash-ref phase-to-top-init phase) 
                                     phase
                                     body-cctx)
                ,@(reverse bodys)]))
        (void))))
  
  (define max-top-init-phase
    (if (zero? (hash-count phase-to-top-init))
        0
        (apply max (hash-keys phase-to-top-init))))
  
  `(begin
    ,@pre-submodules
    (declare-module!
     #:as-submodule? ,as-submodule?
     (current-namespace)
     (module-shift-for-declare
      (let ([,mpi-vector-id ,(generate-module-path-index-deserialize mpis)]
            [,deserialized-syntax-id (make-vector ,(add1 max-top-init-phase) #f)])
        ,module-declaration)
      (substitute-module-declare-name ',root-module-name
                                      ',(resolved-module-path-name
                                         (module-path-index-resolve self)))))
    ,@post-submodules))

;; ----------------------------------------
  
(define (make-top-init mpis)
  (top-init mpis
            (make-variable-uses)
            (box null)
            0))

(define (make-variable-uses)
  (make-hash))

(define (add-syntax-literal! top-init q)
  (define pos (top-init-num-syntax-literals top-init))
  (set-top-init-num-syntax-literals! top-init (add1 pos))
  (define b (top-init-syntax-literals top-init))
  (set-box! b (cons q (unbox b)))
  pos)

(define (generate-top-init top-init phase cctx)
  (append
   (generate-variable-uses! top-init (top-init-variable-uses top-init) phase cctx)
   (generate-syntax-literals! top-init
                              (unbox (top-init-syntax-literals top-init))
                              (top-init-num-syntax-literals top-init)
                              phase
                              cctx)))

(define (generate-variable-uses! top-init variable-uses phase cctx)
  (define in-mod? (compile-context-self cctx))
  (define mpis (top-init-module-path-indexes top-init))
  (define (ref->key ref)
    (cons (module-path-index-resolve (variable-use-module ref))
          (variable-use-phase ref)))
  (define module-nss
    (for/fold ([module-nss #hash()]) ([(ref) (in-hash-keys variable-uses)])
      (define key (ref->key ref))
      (if (hash-ref module-nss key #f)
          module-nss
          (hash-set module-nss key (cons (gensym) (variable-use-module ref))))))
  (append
   (for/list ([(key m-ns-sym+mpi) (in-hash module-nss)])
     (define binding-module (cdr m-ns-sym+mpi))
     (define binding-phase (cdr key))
     `(define ,(car m-ns-sym+mpi)
       (namespace->module-namespace #:complain-on-failure? #t
                                    ,(if in-mod?
                                         ns-id
                                         (compile-context-namespace cctx))
                                    ,(if in-mod?
                                         `(module-path-index-resolve
                                           (module-path-index-shift
                                            ,(add-module-path-index! mpis binding-module)
                                            ,(add-module-path-index! mpis (compile-context-self cctx))
                                            ,self-id))
                                         `',(module-path-index-resolve
                                             binding-module))
                                    ,(let ([phase (phase- phase binding-phase)])
                                       (if in-mod?
                                           `(+ ,phase-shift-id ,phase)
                                           phase)))))
   (for/list ([(ref box-sym) (in-hash variable-uses)])
     (define m-ns-sym+mpi (hash-ref module-nss (ref->key ref)))
     (define m-ns-sym (car m-ns-sym+mpi))
     (define mpi (cdr m-ns-sym+mpi))
     (define sym (variable-use-sym ref))
     `(define ,box-sym 
       (namespace-get-variable-box ,m-ns-sym
                                   ,(variable-use-phase ref)
                                   ',sym
                                   (lambda ()
                                     ,(if (eq? mpi (compile-context-compile-time-for-self cctx))
                                          `(begin
                                            (namespace-set-variable! ,m-ns-sym
                                                                     ,(variable-use-phase ref)
                                                                     ',sym
                                                                     ',undefined)
                                            (namespace-get-variable-box ,m-ns-sym
                                                                        ,(variable-use-phase ref)
                                                                        ',sym
                                                                        #f))
                                          `(begin
                                            (error "link failed:"
                                                   ',sym
                                                   ,(variable-use-phase ref)
                                                   ,m-ns-sym)))))))))

(define (generate-syntax-literals! top-init syntax-literals num-syntax-literals phase cctx)
  (cond
   [(zero? num-syntax-literals)
    null]
   [else
    `((define ,syntax-literals-id (make-vector ,num-syntax-literals))
      (define (,get-syntax-literal!-id pos)
        (unless (vector-ref ,deserialized-syntax-id ,phase)
          (vector-set! ,deserialized-syntax-id
                       ,phase
                       ,(generate-deserialize (vector->immutable-vector
                                               (list->vector (reverse syntax-literals)))
                                              (top-init-module-path-indexes top-init))))
        (define stx
          (syntax-module-path-index-shift
           (syntax-shift-phase-level (vector-ref (vector-ref ,deserialized-syntax-id ,phase) pos) ,phase-shift-id)
           ,(add-module-path-index! (top-init-module-path-indexes top-init) (compile-context-self cctx))
           ,self-id
           ,bulk-binding-registry-id))
        (vector-set! ,syntax-literals-id pos stx)
        stx))]))

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
    (define b (resolve+shift id phase #:immediate? #t))
    (unless (and (module-binding? b)
                 (eq? self (module-binding-module b))
                 (eqv? phase (module-binding-phase b)))
      (error "bad binding for module definition:" id
             self "vs." (module-binding-module b)))
    (module-binding-sym b)))

(define (convert-def-sym sym def-syms)
  (or (hash-ref def-syms sym #f)
      (let ([def-sym (gensym sym)])
        (hash-set! def-syms sym def-sym)
        def-sym)))

(define (convert-def-syms syms def-syms)
  (for/list ([sym (in-list syms)])
    (convert-def-sym sym def-syms)))

;; ----------------------------------------

(define undefined (gensym 'undefined))

(define (check-defined val sym)
  (if (eq? val undefined)
      (error "use before definition:" sym)
      val))

;; ----------------------------------------

(define expand-time-namespace (make-base-namespace))
(define (add-expand-time! sym val)
  (namespace-set-variable-value! sym val #t expand-time-namespace))

(add-deserialize-variables! expand-time-namespace)
(add-expand-time! 'current-namespace current-namespace)
(add-expand-time! 'namespace->module-namespace namespace->module-namespace)
(add-expand-time! 'namespace-get-variable namespace-get-variable)
(add-expand-time! 'namespace-get-variable-box namespace-get-variable-box)
(add-expand-time! 'namespace-set-variable! namespace-set-variable!)
(add-expand-time! 'syntax-shift-phase-level syntax-shift-phase-level)
(add-expand-time! 'module-path-index-join module-path-index-join)
(add-expand-time! 'module-path-index-shift module-path-index-shift)
(add-expand-time! 'module-path-index-resolve module-path-index-resolve)
(add-expand-time! 'check-defined check-defined)
(add-expand-time! 'variable-reference variable-reference)

(define run-time-namespace (make-base-namespace))
(define (add-run-time! sym val)
  (namespace-set-variable-value! sym val #t run-time-namespace))

(add-deserialize-variables! run-time-namespace)
(add-run-time! 'make-module make-module)
(add-run-time! 'declare-module! declare-module!)
(add-run-time! 'current-namespace current-namespace)
(add-run-time! 'namespace-set-variable! namespace-set-variable!)
(add-run-time! 'namespace-set-transformer! namespace-set-transformer!)
(add-run-time! 'namespace-get-variable namespace-get-variable)
(add-run-time! 'namespace-get-variable-box namespace-get-variable-box)
(add-run-time! 'namespace->module-namespace namespace->module-namespace)
(add-run-time! 'syntax-shift-phase-level syntax-shift-phase-level)
(add-run-time! 'module-path-index-join module-path-index-join)
(add-run-time! 'module-path-index-shift module-path-index-shift)
(add-run-time! 'module-path-index-resolve module-path-index-resolve)
(add-run-time! 'module-shift-for-declare module-shift-for-declare)
(add-run-time! 'substitute-module-declare-name substitute-module-declare-name)
(add-run-time! 'syntax-module-path-index-shift syntax-module-path-index-shift)
(add-run-time! 'variable-reference variable-reference)

(define orig-eval (current-eval))

(define (expand-time-eval compiled)
  (parameterize ([base:current-namespace expand-time-namespace])
    (orig-eval compiled)))

(define (run-time-eval compiled)
  (parameterize ([base:current-namespace run-time-namespace])
    (orig-eval compiled)))

(define (compile-install-primitives! ns mod-name)
  (namespace-module-instantiate! ns mod-name 0)
  (define m (namespace->module ns mod-name))
  (define m-ns (namespace->module-namespace ns mod-name 0))
  (for ([(sym b) (in-hash (hash-ref (module-provides m) 0))])
    (define val (namespace-get-variable m-ns 0 (module-binding-sym b) #f))
    (when val
      (namespace-set-variable-value! sym val #t expand-time-namespace)
      (namespace-set-variable-value! sym val #t run-time-namespace))))
