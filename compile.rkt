#lang racket/base
(require "set.rkt"
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "namespace.rkt"
         "binding.rkt"
         "match.rkt"
         "core.rkt"
         "require+provide.rkt"
         "module-path.rkt"
         "module-use.rkt"
         "expand-require.rkt"
         "variable-reference.rkt"
         "serialize.rkt"
         "linklet.rkt"
         "side-effect.rkt"
         "built-in-symbol.rkt"
         (only-in racket/base
                  [current-namespace base:current-namespace]
                  [compile base:compile]))

(provide make-compile-context
         compile-top
         compile-module
         
         compiled-top?

         declare-module-from-linklet-directory!
         run-top-level-from-linklet-directory
         compiled-top-run)

(struct compile-context (namespace   ; compile-time namespace
                         phase       ; phase (top level) or phase level (within a module)
                         self        ; if non-#f module path index, compiling the body of a module
                         compile-time-for-self ; for allowing forward references across `begin-for-syntax`
                         root-module-name ; set to a symbol if `self` is non-#f
                         top-init))  ; accumulates top-level initialization

(struct top-init (module-path-indexes        ; module-path-index -> linklet import position
                  binding-sym-to-define-sym  ; sym -> sym; avoid conflicts with primitives
                  [binding-syms-in-order #:mutable] ; list of sym
                  require-var-to-import-sym  ; variable-use -> sym
                  [require-vars-in-order #:mutable] ; list of variable-use
                  define-and-import-syms     ; hash of sym, to select distinct symbols
                  syntax-literals            ; box of list of syntax-literal
                  [num-syntax-literals #:mutable]))

(struct variable-use (module-use sym)
        #:transparent) ; for hashing

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
                   #f))

(define phase-shift-id (make-built-in-symbol! 'phase))
(define ns-id (make-built-in-symbol! 'namespace))
(define self-id (make-built-in-symbol! 'self))
(define syntax-literals-id (make-built-in-symbol! 'syntax-literals))
(define get-syntax-literal!-id (make-built-in-symbol! 'get-syntax-literal!))
(define bulk-binding-registry-id (make-built-in-symbol! 'bulk-binding-registry))
(define deserialized-syntax-id (make-built-in-symbol! 'deserialized-syntax))
(define set-transformer!-id (make-built-in-symbol! 'set-transformer!))
(define body-thunk-id (make-built-in-symbol! 'body-thunk))

;; ----------------------------------------

(define instance-imports
  `([namespace ,ns-id]
    [phase-shift ,phase-shift-id]
    [self ,self-id]
    [bulk-binding-registry ,bulk-binding-registry-id]
    [set-transformer! ,set-transformer!-id]))

(define (make-instance-instance #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:bulk-binding-registry bulk-binding-registry
                                #:set-transformer! set-transformer!)
  (define i (make-instance 'instance))
  (set-instance-variable-value! i 'namespace ns)
  (set-instance-variable-value! i 'phase-shift phase-shift)
  (set-instance-variable-value! i 'self self)
  (set-instance-variable-value! i 'bulk-binding-registry bulk-binding-registry)
  (set-instance-variable-value! i 'set-transformer! set-transformer!)
  i)

;; ----------------------------------------

(struct compiled-top (linklet-directory
                      phase
                      link-module-uses
                      get-mpis
                      get-syntax-literals))

;; Returns a linking directory with two linking units
(define (compile-top s cctx
                     #:serializable? [serializable? #t])
  (define mpis (make-module-path-index-table))
  (define top-init (make-top-init mpis))
  (define phase (compile-context-phase cctx))
  (define compiled
    (compile s (struct-copy compile-context cctx
                            [top-init top-init])))
  (define-values (link-module-uses imports def-decls)
    (generate-links+imports top-init phase cctx))
  
  (define cu
    (compile-linklet
     `(linklet
       #:import ([instance ,@instance-imports]
                 [link (mpi-vector ,mpi-vector-id)
                       (syntax-literals ,syntax-literals-id)
                       (get-syntax-literal! ,get-syntax-literal!-id)]
                 ,@imports)
       #:export (,@def-decls
                 [,body-thunk-id body-thunk])
       (define-values (,body-thunk-id) (lambda () ,compiled)))))
  
  (define code
    (cond
     [serializable?
      (define syntax-literals
        (generate-syntax-literals! (top-init-syntax-literals top-init)
                                   mpis
                                   phase
                                   (compile-context-self cctx)))
      (define link-module-use-exprs
        (serialize-module-uses link-module-uses mpis))
      
      (define link-cu
        (compile-linklet
         `(linklet
           #:import ([deserialize ,@deserialize-imports])
           #:export ([,mpi-vector-id mpi-vector]
                     deserialized-syntax
                     link-modules
                     original-phase)
           (define-values (,mpi-vector-id)
             ,(generate-module-path-index-deserialize mpis))
           (define-values (deserialized-syntax) 
             (make-vector ,(add1 phase) #f))
           (define-values (original-phase) ,phase)
           (define-values (link-modules)
             (list ,@link-module-use-exprs))
           ,@syntax-literals)))
      
      (hash->linklet-directory
       (hash #"top" cu
             #"link" link-cu))]
     [else
      ;; Will combine the linking unit with non-serilized link info
      (hash->linklet-directory
       (hash #"top" cu))]))
  
  ;; If the compiled code is executed directly in its original phase,
  ;; we'll share the original values
  (compiled-top code
                phase
                link-module-uses
                (mpis-as-vector-getter mpis)
                (syntax-literals-as-vector-getter (top-init-syntax-literals top-init))))

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
         (error "not a core expression form:" s)]
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
         (define pos (add-syntax-literal! (compile-context-top-init cctx) q))
         `(let-values ([(stx) (vector-ref ,syntax-literals-id ,pos)])
           (if stx
               stx
               (,get-syntax-literal!-id ,pos)))]
        [(#%variable-reference)
         (define id-m (try-match-syntax s '(#%variable-reference id)))
         (define top-m (and (not id-m)
                            (try-match-syntax s '(#%variable-reference (#%top . id)))))
         (define id (or (and id-m (id-m 'id))
                        (and top-m (top-m 'id))))
         (if id
             `(#%variable-reference ,(compile-identifier id cctx))
             `(#%variable-reference))]
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
  (define sym
    (cond
     [(local-binding? b)
      (define sym (key->symbol (local-binding-key b)))
      (unless sym
        (error "missing a binding after expansion:" s))
      sym]
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
        (namespace-module-instantiate! ns mpi 0)
        (define m-ns (namespace->module-namespace ns mod-name 0))
        ;; Expect each primitive to be bound:
        (module-binding-sym b)]
       [(eq? mpi (compile-context-self cctx))
        ;; Direct reference to a variable defined in the same module:
        (define top-init (compile-context-top-init cctx))
        (hash-ref (top-init-binding-sym-to-define-sym top-init)
                  (module-binding-sym b))]
       [else
        ;; Reference to a variable defined in another module; register
        ;; as a linklet import
        (define key (variable-use (module-use (module-binding-module b)
                                              (module-binding-phase b))
                                  (module-binding-sym b)))
        (define top-init (compile-context-top-init cctx))
        (define variable-uses (top-init-require-var-to-import-sym top-init))
        (or (hash-ref variable-uses key #f)
            (let ([sym (select-fresh (variable-use-sym key) top-init)])
              (hash-set! variable-uses key sym)
              (set-top-init-require-vars-in-order! top-init
                                                   (cons key
                                                         (top-init-require-vars-in-order top-init)))
              (hash-set! (top-init-define-and-import-syms top-init) sym #t)
              sym))])]
     [else
      (error "not a reference to a module or local binding:" s)]))
  (if rhs
      `(set! ,sym ,rhs)
      sym))

;; ----------------------------------------

;; Returns a linking directory
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
  
  ;; Select non-conflicting symbols for definitions, first, in the
  ;; hope that we can just the names as-is; and we'll rename locals as
  ;; needed to avoid these names
  (let loop! ([bodys bodys] [phase 0] [top-init (find-or-create-top-init! 0)])
    (for ([body (in-list bodys)])
      (case (core-form-sym body phase)
        [(define-values)
         (define m (match-syntax body '(define-values (id ...) rhs)))
         (for ([sym (in-list (def-ids-to-binding-syms (m 'id) phase self))])
           (define def-sym (select-fresh sym top-init))
           (hash-set! (top-init-binding-sym-to-define-sym top-init)
                      sym
                      def-sym)
           (set-top-init-binding-syms-in-order! top-init
                                                (cons sym
                                                      (top-init-binding-syms-in-order top-init)))
           (hash-set! (top-init-define-and-import-syms top-init) def-sym #t))]
         [(begin-for-syntax)
          (define m (match-syntax body `(begin-for-syntax e ...)))
          (loop! (m 'e) (add1 phase) (find-or-create-top-init! (add1 phase)))])))

  (define body-cctx (struct-copy compile-context cctx
                                 [self self]
                                 [root-module-name root-module-name]))
  
  ;; Phases with side effects
  (define side-effects (make-hasheqv))
  
  (define (check-side-effects! e ; compiled expression
                               expected-results ; number of expected reuslts, or #f if any number is ok
                               phase)
    (when (any-side-effects? e expected-results)
      (hash-set! side-effects phase #t)))
  
  (let loop! ([bodys bodys] [phase 0] [top-init (find-or-create-top-init! 0)])
    (for ([body (in-list bodys)])
      (case (core-form-sym body phase)
        [(define-values)
         (define m (match-syntax body '(define-values (id ...) rhs)))
         (define binding-syms (def-ids-to-binding-syms (m 'id) phase self))
         (define def-syms (for/list ([binding-sym (in-list binding-syms)])
                            (hash-ref (top-init-binding-sym-to-define-sym top-init)
                                      binding-sym)))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context body-cctx
                                           [phase phase]
                                           [top-init top-init])))
         (check-side-effects! rhs (length def-syms) phase)
         (add-body! phase `(define-values ,def-syms ,rhs))]
        [(define-syntaxes)
         (define m (match-syntax body '(define-syntaxes (id ...) rhs)))
         (define binding-syms (def-ids-to-binding-syms (m 'id) phase self))
         (define next-top-init (find-or-create-top-init! (add1 phase)))
         (define gen-syms (for/list ([binding-sym (in-list binding-syms)])
                            (select-fresh binding-sym next-top-init)))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context body-cctx
                                           [phase (add1 phase)]
                                           [top-init next-top-init])))
         (check-side-effects! rhs (length gen-syms) (add1 phase))
         (add-body! (add1 phase) `(let-values ([,gen-syms ,rhs])
                                   ,@(for/list ([binding-sym (in-list binding-syms)]
                                                [gen-sym (in-list gen-syms)])
                                       `(,set-transformer!-id ',binding-sym ,gen-sym))
                                   (void)))]
        [(begin-for-syntax)
         (define m (match-syntax body `(begin-for-syntax e ...)))
         (loop! (m 'e) (add1 phase) (find-or-create-top-init! (add1 phase)))]
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
         (define e (compile body
                            (struct-copy compile-context body-cctx
                                         [phase phase]
                                         [top-init top-init])))
         (check-side-effects! e #f phase)
         (add-body! phase e)])))
  
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
            (define sm-m (match-syntax (car bodys) '(_ name . _)))
            (define s-shifted
              (cond
               [(try-match-syntax (car bodys) '(module* name #f . _))
                (syntax-shift-phase-level (car bodys) (phase- 0 phase))]
               [else (car bodys)]))
            (cons (cons (syntax-e (sm-m 'name))
                        (compile-module s-shifted body-cctx))
                  (loop (cdr bodys) phase))]
           [(eq? f 'begin-for-syntax)
            (define m (match-syntax (car bodys) `(begin-for-syntax e ...)))
            (append (loop (m 'e) (add1 phase))
                    (loop (cdr bodys) phase))]
          [else
           (loop (cdr bodys) phase)])]))]))

  ;; Each list is (cons linklet-directory-key linklet-directory)
  (define pre-submodules (compile-submodules 'module))
  (define post-submodules (compile-submodules 'module*))

  (define phases-in-order (sort (hash-keys phase-to-body) <))

  (define-values (min-phase max-phase)
    (for/fold ([min-phase 0] [max-phase 0]) ([phase (in-hash-keys phase-to-body)])
      (values (min min-phase phase)
              (max max-phase phase))))
  
  ;; Compute linking info for each phase
  (struct link-info (link-module-uses imports))
  (define phase-to-link-info
    (for/hash ([phase (in-list phases-in-order)])
      (define top-init (hash-ref phase-to-top-init phase))
      (define-values (link-module-uses imports def-decls)
        (generate-links+imports top-init phase body-cctx))
      (unless (null? def-decls)
        (error "internal error definition declarations for closed module"))
      (values phase (link-info link-module-uses imports))))

  ;; Generate module-declaration info, which includes linking
  ;; information for each phase
  (define declaration-body
    `((define-values (self-mpi) ,(add-module-path-index! mpis self))
      (define-values (default-name) ',(resolved-module-path-name
                                       (module-path-index-resolve self)))
      (define-values (root-module-name) ',root-module-name)
      (define-values (cross-phase-persistent?) ,cross-phase-persistent?)
      (define-values (requires) ,(generate-deserialize requires mpis))
      (define-values (provides) ,(generate-deserialize provides mpis))
      (define-values (side-effects) ',(sort (hash-keys side-effects) <))
      (define-values (min-phase) ,min-phase)
      (define-values (max-phase) ,max-phase)
      (define-values (phase-to-link-modules)
        ;; For each phase, the modules to link as imports
        (hash ,@(apply
                 append
                 (for/list ([phase (in-list phases-in-order)])
                   (list phase `(list ,@(serialize-module-uses (link-info-link-module-uses
                                                                (hash-ref phase-to-link-info phase null))
                                                               mpis)))))))
      (define-values (pre-submodules)
        ',(map car pre-submodules))
      (define-values (post-submodules)
        ',(map car post-submodules))))

  ;; Generate the phase-specific linking units
  (define body-linklets
    (for/hash ([phase (in-list phases-in-order)])
      (define bodys (hash-ref phase-to-body phase))
      (define li (hash-ref phase-to-link-info phase))
      (define syntax-literals (top-init-syntax-literals (hash-ref phase-to-top-init phase)))
      (define binding-sym-to-define-sym
        (top-init-binding-sym-to-define-sym (hash-ref phase-to-top-init phase)))
      (values
       (encode-linklet-directory-key phase)
       (compile-linklet
        `(linklet
          #:import ([deserialize ,@(if (empty-syntax-literals? syntax-literals)
                                       null
                                       deserialize-imports)]
                    [declaration (mpi-vector ,mpi-vector-id)
                                 (deserialized-syntax ,deserialized-syntax-id)]
                    [instance ,@instance-imports]
                    ,@(link-info-imports li))
          #:export (,@(for/list ([binding-sym (in-list (top-init-binding-syms-in-order
                                                        (hash-ref phase-to-top-init phase)))])
                        (define def-sym (hash-ref binding-sym-to-define-sym binding-sym))
                        `[,def-sym ,binding-sym]))
          ,@(generate-syntax-literals! syntax-literals mpis phase self)
          ,@(reverse bodys))))))

  (define max-top-init-phase
    (if (zero? (hash-count phase-to-top-init))
        0
        (apply max (hash-keys phase-to-top-init))))

  ;; Assemble the declaration linking unit, which is instanted
  ;; once for a module declaration and shared among instances
  (define declaration-linklet
    (compile-linklet
     `(linklet
       #:import ([deserialize ,@deserialize-imports])
       #:export (self-mpi
                 default-name
                 root-module-name
                 requires
                 provides
                 variables
                 side-effects
                 cross-phase-persistent?
                 min-phase
                 max-phase
                 phase-to-link-modules
                 pre-submodules
                 post-submodules
                 [,mpi-vector-id mpi-vector]
                 deserialized-syntax)
       (define-values (,mpi-vector-id)
         ,(generate-module-path-index-deserialize mpis))
       (define-values (deserialized-syntax)
         (make-vector ,(add1 max-top-init-phase) #f))
       ,@declaration-body)))

  (hash->linklet-directory
   (for/fold ([ht (hash-set body-linklets #"" declaration-linklet)])
             ([sm (in-list (append pre-submodules post-submodules))])
     (hash-set ht (encode-linklet-directory-key (car sm)) (cdr sm)))))

;; ----------------------------------------
  
(define (make-top-init mpis)
  (top-init mpis
            (make-hasheq)        ; binding-sym-to-define-sym
            null                 ; binding-syms-in-order
            (make-variable-uses) ; require-var-to-import-sym
            null                 ; require-vars-in-order
            (make-hasheq)        ; define-and-import-syms
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

;; Returns:
;;  link-names : a list of sym
;;  link-requires : a list of module path indexes
;;  imports : a list of S-expressions for imports; refers to `link-names`
;;  def-decls : a list of S-expressions for forward-reference declarations
(define (generate-links+imports top-init phase cctx)
  ;; Make a link symbol for each distinct module+phase:
  (define mod-use-to-link-sym
    (for/fold ([ht #hash()]) ([(vu) (in-list (top-init-require-vars-in-order top-init))])
      (define mu (variable-use-module-use vu))
      (if (or (hash-ref ht mu #f)
              (eq? (module-use-module mu)
                   (compile-context-compile-time-for-self cctx)))
          ht
          (hash-set ht mu (string->symbol
                           (format "~a_~a_~a"
                                   (extract-name (module-use-module mu))
                                   (module-use-phase mu)
                                   (hash-count ht)))))))
  ;; List of distinct module+phases:
  (define link-mod-uses (hash-keys mod-use-to-link-sym))

  (values
   ;; Module-uses list:
   link-mod-uses
   ;; Imports, using the same order as module-uses list:
   (for/list ([mu (in-list link-mod-uses)])
     (cons (hash-ref mod-use-to-link-sym mu)
           (for/list ([vu (in-list (top-init-require-vars-in-order top-init))]
                      #:when (equal? mu (variable-use-module-use vu)))
           (define var-sym (hash-ref (top-init-require-var-to-import-sym top-init) vu))
             `[,(variable-use-sym vu) ,var-sym])))
   ;; Declarations (for non-module contexts)
   (for/list ([vu (in-list (top-init-require-vars-in-order top-init))]
              #:when (eq? (module-use-module (variable-use-module-use vu))
                          (compile-context-compile-time-for-self cctx)))
     (define var-sym (hash-ref (top-init-require-var-to-import-sym top-init) vu))
     `(,var-sym ,(variable-use-sym vu)))))

(define (empty-syntax-literals? syntax-literals)
  (null? (unbox syntax-literals)))

(define (generate-syntax-literals! syntax-literals-box mpis phase self)
  (define syntax-literals (unbox syntax-literals-box))
  (define len (length syntax-literals))
  (cond
   [(zero? len) null]
   [else
    `((define-values (,syntax-literals-id) (make-vector ,len #f))
      (define-values (,get-syntax-literal!-id)
        (lambda (pos)
          (begin
            (if (vector-ref ,deserialized-syntax-id ,phase)
                (void)
                (vector-set! ,deserialized-syntax-id
                             ,phase
                             ,(generate-deserialize (vector->immutable-vector
                                                     (list->vector (reverse syntax-literals)))
                                                    mpis)))
            (let-values ([(stx)
                          (syntax-module-path-index-shift
                           (syntax-shift-phase-level
                            (vector-ref (vector-ref ,deserialized-syntax-id ,phase) pos) ,phase-shift-id)
                           ,(add-module-path-index! mpis self)
                           ,self-id
                           ,bulk-binding-registry-id)])
              (begin
                (vector-set! ,syntax-literals-id pos stx)
                stx))))))]))

(define (syntax-literals-as-vector-getter syntax-literals-box)
  (define syntax-literals (unbox syntax-literals-box))
  (define vec (list->vector (reverse syntax-literals)))
  (lambda (phase-shift)
    (if (zero? phase-shift)
        vec
        (for/vector #:length (vector-length vec) ([s (in-vector vec)])
                    (syntax-shift-phase-level s phase-shift)))))

;; ----------------------------------------

(define (def-ids-to-binding-syms ids phase self)
  (for/list ([id (in-list ids)])
    (define b (resolve+shift id phase #:immediate? #t))
    (unless (and (module-binding? b)
                 (eq? self (module-binding-module b))
                 (eqv? phase (module-binding-phase b)))
      (error "bad binding for module definition:" id
             self "vs." (module-binding-module b)))
    (module-binding-sym b)))

(define (select-fresh sym top-init)
  (if (symbol-conflicts? sym top-init)
      (let loop ([pos 1])
        (define new-sym (string->symbol (format "~a/~a" pos sym)))
        (if (symbol-conflicts? new-sym top-init)
            (loop (add1 pos))
            new-sym))
      sym))

(define (symbol-conflicts? sym top-init)
  (or (built-in-symbol? sym)
      (hash-ref (top-init-define-and-import-syms top-init) sym #f)))

(define (local->symbol id phase)
  (define b (resolve id phase))
  (unless (local-binding? b)
    (error "bad binding:" id))
  (key->symbol (local-binding-key b)))

(define (key->symbol key)
  ;; A local-binding key is already an distinct uninterned symbol
  ;; (with a deterministic label)
  key)

;; Get a reasonably nice name from a module-path-index
(define (extract-name mpi)
  (define-values (p base) (module-path-index-split mpi))
  (cond
   [(symbol? p) p]
   [(path? p) (let-values ([(base name dir?) (split-path p)])
                (path-replace-extension name #""))]
   [(string? p) (path-replace-extension p #"")]
   [(and (pair? p) (eq? (car p) 'quote))
    (cadr p)]
   [(and (pair? p) (eq? (car p) 'file))
    (let-values ([(base name dir?) (split-path (cadr p))])
      (path-replace-extension name #""))]
   [(and (pair? p) (eq? (car p) 'lib))
    (path-replace-extension (cadr p) #"")]
   [else 'module]))
 
;; ----------------------------------------

(define (eval-linklets h)
  (for/hash ([(name v) (in-hash h)])
    (values name
            (if (linklet-directory? v)
                v
                (eval-linklet v)))))

;; ----------------------------------------

(define (declare-module-from-linklet-directory! cd
                                                #:namespace [ns (current-namespace)]
                                                #:as-submodule? [as-submodule? #f])
  (define h (eval-linklets (linklet-directory->hash cd)))
  (define declaration-instance
    (instantiate-linklet (hash-ref h #"")
                         (list deserialize-instance)))
  
  (define (decl key)
    (instance-variable-value declaration-instance key))
  
  (define (declare-submodules names)
    (for ([name (in-list names)])
      (define sm-cd (hash-ref h (encode-linklet-directory-key name)))
      (unless sm-cd (error "missing submodule declaration:" name))
      (declare-module-from-linklet-directory! sm-cd #:namespace ns)))
  
  (unless as-submodule?
    (declare-submodules (decl 'pre-submodules)))
  
  (define root-module-name (instance-variable-value declaration-instance 'root-module-name))
  
  (define original-self (decl 'self-mpi))
   
  (define m (make-module original-self
                         (decl 'requires)
                         (decl 'provides)
                         (decl 'min-phase)
                         (decl 'max-phase)
                         (lambda (ns phase-shift phase-level self bulk-binding-registry)
                           (define cu (hash-ref h (encode-linklet-directory-key phase-level) #f))
                           (when cu
                             (define imports
                               (for/list ([mu (in-list (hash-ref (decl 'phase-to-link-modules) phase-level))])
                                 (namespace-module-use->instance ns mu
                                                                 #:shift-from original-self
                                                                 #:shift-to self
                                                                 #:phase-shift
                                                                 (phase+ (phase- phase-level (module-use-phase mu))
                                                                         phase-shift))))
                             (define inst
                               (make-instance-instance
                                #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:bulk-binding-registry bulk-binding-registry
                                #:set-transformer! (lambda (name val)
                                                     (namespace-set-transformer! ns (sub1 phase-level) name val))))
                             (instantiate-linklet cu (list* deserialize-instance
                                                            declaration-instance
                                                            inst
                                                            imports)
                                                  (namespace->instance ns phase-level))))
                         #:cross-phase-persistent? (decl 'cross-phase-persistent?)))

  (declare-module! ns
                   m
                   (substitute-module-declare-name (decl 'root-module-name)
                                                   (decl 'default-name))
                   #:as-submodule? as-submodule?)

  (unless as-submodule?
    (declare-submodules (decl 'post-submodules))))

;; ----------------------------------------

(define (run-top-level-from-linklet-directory cd ns)
  (compiled-top-run (compiled-top cd #f #f #f #f)))

(define (compiled-top-run ct ns)
  (define cd (compiled-top-linklet-directory ct))
  (define h (eval-linklets (linklet-directory->hash cd)))
  (define link-instance
    (and (not (compiled-top-link-module-uses ct))
         (instantiate-linklet (hash-ref h #"link")
                              (list deserialize-instance))))
  (define phase (namespace-phase ns))
  (define imports
    (for/list ([mu (or (compiled-top-link-module-uses ct)
                       (instance-variable-value link-instance 'link-modules))])
      (namespace-module-use->instance ns mu #:phase-shift (phase- phase (module-use-phase mu)))))
  
  (define phase-shift (phase- (namespace-phase ns)
                              (or (compiled-top-phase ct)
                                  (instance-variable-value link-instance 'original-phase))))
  
  (define inst (make-instance-instance
                #:namespace ns
                #:phase-shift phase-shift
                #:self #f ; FIXME
                #:bulk-binding-registry (namespace-bulk-binding-registry ns)
                #:set-transformer! (lambda (name val)
                                     (namespace-set-transformer! ns phase-shift name val))))

  (define i
    (instantiate-linklet (hash-ref h #"top")
                         (list* inst
                                (or link-instance
                                    (compiled-top-make-link-instance ct phase-shift))
                                imports)
                         ;; Instantiation merges with the namespace's current instance:
                         (namespace->instance ns (namespace-phase ns))))
  
  ((instance-variable-value i 'body-thunk)))
  

(define (compiled-top-make-link-instance ct phase-shift)
  (define link-instance (make-instance 'top))
  (set-instance-variable-value! link-instance 'mpi-vector
                                ((compiled-top-get-mpis ct)))
  (set-instance-variable-value! link-instance 'syntax-literals
                                ((compiled-top-get-syntax-literals ct) phase-shift))
  (set-instance-variable-value! link-instance 'get-syntax-literal!
                                (lambda (pos)
                                  (error "internal error: missing syntax literal?" pos)))
  link-instance)
