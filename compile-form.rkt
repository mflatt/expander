#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "phase.rkt"
         "core.rkt"
         "namespace.rkt"
         "root-expand-context.rkt"
         "module-path.rkt"
         "module-use.rkt"
         "serialize.rkt"
         "side-effect.rkt"
         "built-in-symbol.rkt"
         "linklet.rkt"
         "compile-context.rkt"
         "compile-header.rkt"
         "compile-impl-id.rkt"
         "compile-def-id.rkt"
         "compile-instance.rkt"
         "compile-namespace-scope.rkt"
         "compile-expr.rkt")

(provide compile-forms

         compile-namespace-scopes)

;; Compiles a module body or sequence of top-level forms, returning a
;; linklet directory to cover all phases covered by the forms
(define (compile-forms bodys cctx mpis
                       #:phase-in-body-thunk [phase-in-body-thunk #f] ; phase, if any, to export `body-thunk`
                       #:compiled-expression-callback [compiled-expression-callback void]
                       #:other-form-callback [other-form-callback void])
  (define phase (compile-context-phase cctx))
  (define self (compile-context-self cctx))

  ;; For each phase, keep track of all compiled expressions for the
  ;; phase
  (define phase-to-body (make-hasheqv)) ; phase -> list of S-expression
  (define (add-body! phase body)
    (hash-update! phase-to-body phase (lambda (l) (cons body l)) null))

  ;; For each phase, accumulate a header for referenced imports and
  ;; syntax literals
  (define phase-to-header (make-hasheqv)) ; phase -> header
  (define (find-or-create-header! phase)
    (or (hash-ref phase-to-header phase #f)
        (let ([header (make-header mpis)])
          (hash-set! phase-to-header phase header)
          header)))
  
  (when (compile-context-module-self cctx)
    ;; In a module, select non-conflicting symbols for definitions,
    ;; first, in the hope that we can just the names as-is; and we'll
    ;; rename locals as needed to avoid these names
    (let loop! ([bodys bodys] [phase phase] [header (find-or-create-header! phase)])
      (for ([body (in-list bodys)])
        (case (core-form-sym body phase)
          [(define-values)
           (define m (match-syntax body '(define-values (id ...) rhs)))
           (for ([sym (in-list (def-ids-to-binding-syms (m 'id) phase self))])
             (define def-sym (select-fresh sym header))
             (hash-set! (header-binding-sym-to-define-sym header)
                        sym
                        def-sym)
             (set-header-binding-syms-in-order! header
                                                (cons sym
                                                      (header-binding-syms-in-order header)))
             (hash-set! (header-define-and-import-syms header) def-sym #t))]
          [(begin-for-syntax)
           (define m (match-syntax body `(begin-for-syntax e ...)))
           (loop! (m 'e) (add1 phase) (find-or-create-header! (add1 phase)))]))))

  ;; Compile each form in `bodys`, recording results in `phase-to-body`
  (let loop! ([bodys bodys] [phase phase] [header (find-or-create-header! phase)])
    (for ([body (in-list bodys)])
      (case (core-form-sym body phase)
        [(define-values)
         (define m (match-syntax body '(define-values (id ...) rhs)))
         (define ids (m 'id))
         (define binding-syms (def-ids-to-binding-syms ids phase self))
         (define def-syms
           (cond
            [(compile-context-module-self cctx)
             ;; In a module, look up name for local definition:
             (for/list ([binding-sym (in-list binding-syms)])
               (hash-ref (header-binding-sym-to-define-sym header)
                         binding-sym))]
            [else
             ;; Outside of a module, look up name to `set!`
             (for/list ([binding-sym (in-list binding-syms)])
               (register-required-variable-use! header
                                                (compile-context-self cctx)
                                                phase
                                                binding-sym))]))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context cctx
                                           [phase phase]
                                           [header header])))
         (compiled-expression-callback rhs (length def-syms) phase)
         (cond
          [(compile-context-module-self cctx)
           ;; In a module, generate a definition:
           (add-body! phase `(define-values ,def-syms ,rhs))]
          [else
           ;; Not in a module, generate a set of assignments
           (define gen-syms (for/list ([binding-sym (in-list binding-syms)])
                              (select-fresh binding-sym header)))
           (add-body! phase
                      `(let-values ([,gen-syms ,rhs])
                        ;; Top-level variables are treated as "this variable must exist"
                        ;; declarations at the linklet level, and (re-)defining the
                        ;; variable just mutates it.
                        ,@(for/list ([def-sym (in-list def-syms)]
                                     [gen-sym (in-list gen-syms)])
                            `(set! ,def-sym ,gen-sym))
                        (void)))
           (add-body! phase (compile-top-level-bind
                             ids binding-syms
                             (struct-copy compile-context cctx
                                          [phase phase]
                                          [header header])))])]
        [(define-syntaxes)
         (define m (match-syntax body '(define-syntaxes (id ...) rhs)))
         (define ids (m 'id))
         (define binding-syms (def-ids-to-binding-syms ids phase self))
         (define next-header (find-or-create-header! (add1 phase)))
         (define gen-syms (for/list ([binding-sym (in-list binding-syms)])
                            (select-fresh binding-sym next-header)))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context cctx
                                           [phase (add1 phase)]
                                           [header next-header])))
         (compiled-expression-callback rhs (length gen-syms) (add1 phase))
         (define transformer-set!s (for/list ([binding-sym (in-list binding-syms)]
                                              [gen-sym (in-list gen-syms)])
                                     `(,set-transformer!-id ',binding-sym ,gen-sym)))
         (cond
          [(compile-context-module-self cctx)
           (add-body! (add1 phase) `(let-values ([,gen-syms ,rhs])
                                     ,@transformer-set!s
                                     (void)))]
          [else
           (add-body! (add1 phase)
                      (generate-top-level-define-syntaxes gen-syms rhs transformer-set!s))])
         (unless (compile-context-module-self cctx)
           (add-body! phase (compile-top-level-bind
                             ids binding-syms
                             (struct-copy compile-context cctx
                                          [phase phase]
                                          [header header]))))]
        [(begin-for-syntax)
         (define m (match-syntax body `(begin-for-syntax e ...)))
         (loop! (m 'e) (add1 phase) (find-or-create-header! (add1 phase)))]
        [(#%require #%provide #%declare module module*)
         ;; Must be handled separately, if allowed at all
         (define e (other-form-callback body (struct-copy compile-context cctx
                                                          [phase phase]
                                                          [header header])))
         (when e
           (compiled-expression-callback e #f phase)
           (add-body! phase e))]
        [else
         (define e (compile body
                            (struct-copy compile-context cctx
                                         [phase phase]
                                         [header header])))
         (compiled-expression-callback e #f phase)
         (add-body! phase e)])))

  ;; Collect resulting phases
  (define phases-in-order (sort (hash-keys phase-to-body) <))
  (define min-phase (if (pair? phases-in-order)
                        (car phases-in-order)
                        phase))
  (define max-phase (if (pair? phases-in-order)
                        (car (reverse phases-in-order))
                        phase))

  ;; Compute linking info for each phase
  (struct link-info (link-module-uses imports def-decls))
  (define phase-to-link-info
    (for/hash ([phase (in-list phases-in-order)])
      (define header (hash-ref phase-to-header phase #f))
      (define-values (link-module-uses imports def-decls)
        (generate-links+imports header phase cctx))
      (values phase (link-info link-module-uses imports def-decls))))

  ;; Generate the phase-specific linking units
  (define body-linklets
    (for/hash ([phase (in-list phases-in-order)])
      (define bodys (hash-ref phase-to-body phase))
      (define li (hash-ref phase-to-link-info phase))
      (define embed-syntax-literals? (compile-context-lazy-syntax-literals? cctx))
      (define syntax-literals (and embed-syntax-literals?
                                   (header-syntax-literals (hash-ref phase-to-header phase))))
      (define binding-sym-to-define-sym
        (header-binding-sym-to-define-sym (hash-ref phase-to-header phase)))
      (values
       (encode-linklet-directory-key phase)
       (compile-linklet
        `(linklet
          #:import (,@(if embed-syntax-literals?
                          ;; For modules, put syntax literal deserialization in the
                          ;; module, using a `declaration` import to hold deserialization
                          ;; results for sharing across module instances
                          `([deserialize ,@(if (empty-syntax-literals? syntax-literals)
                                               null
                                               deserialize-imports)]
                            [data (mpi-vector ,mpi-vector-id)
                                  (deserialized-syntax ,deserialized-syntax-id)])
                          ;; For top-level forms, import values that may or may not have
                          ;; been serialized and (eagerly) deserialized
                          `([top-level (top-level-bind! ,top-level-bind!-id)
                                       (top-level-require! ,top-level-require!-id)]
                            [link (mpi-vector ,mpi-vector-id)
                                  (syntax-literalss ,syntax-literalss-id)]))
                    [instance ,@instance-imports]
                    ,@(link-info-imports li))
          #:export (,@(link-info-def-decls li)
                    ,@(for/list ([binding-sym (in-list (header-binding-syms-in-order
                                                        (hash-ref phase-to-header phase)))])
                        (define def-sym (hash-ref binding-sym-to-define-sym binding-sym))
                        `[,def-sym ,binding-sym])
                    ,@(if (eqv? phase phase-in-body-thunk)
                          `([,body-thunk-id body-thunk])
                          null))
          ,@(if embed-syntax-literals?
                (generate-lazy-syntax-literals! syntax-literals mpis phase self)
                null)
          ,@(if (eqv? phase phase-in-body-thunk)
                `[(define-values (,body-thunk-id)
                    (lambda () (begin (void) ,@(reverse bodys))))]
                (reverse bodys)))))))
  
  (define phase-to-link-module-uses
    (for/hash ([(phase li) (in-hash phase-to-link-info)])
      (values phase (link-info-link-module-uses li))))
  
  (define phase-to-link-module-uses-expr
    `(hash ,@(apply
              append
              (for/list ([phase (in-list phases-in-order)])
                (list phase `(list ,@(serialize-module-uses (hash-ref phase-to-link-module-uses phase)
                                                            mpis)))))))
  
  (define syntax-literalss
    (for/list ([phase (in-range phase (add1 max-phase))])
      (define h (hash-ref phase-to-header phase #f))
      (if h
          (header-syntax-literals h)
          empty-syntax-literals)))

  (values body-linklets   ; main compilation result
          min-phase
          max-phase
          phase-to-link-module-uses
          phase-to-link-module-uses-expr
          syntax-literalss))

;; ----------------------------------------

;; Evaluating a top-level definition has a secondary effect: it
;; adjusts the binding of defined identifiers. This mingling of
;; evaluation and expansion is the main weirdness of the top
;; level.
(define (compile-top-level-bind ids binding-syms cctx)
  (define phase (compile-context-phase cctx))
  (define self (compile-context-self cctx))
  (define header (compile-context-header cctx))
  (define mpis (header-module-path-indexes header))
  ;; The binding that we install at run time should not include
  ;; the temporary binding scope that the expander added:
  (define top-level-bind-scope (root-expand-context-top-level-bind-scope
                                (namespace-root-expand-ctx
                                 (compile-context-namespace cctx))))
  ;; For installing a binding:
  (define self-expr (add-module-path-index! mpis self))
  ;; Generate calls to `top-level-bind!`:
  `(begin
    ,@(for/list ([id (in-list ids)]
                 [binding-sym (in-list binding-syms)])
        (define id-stx
          (compile-quote-syntax (remove-scope id top-level-bind-scope)
                                phase
                                cctx))
        `(,top-level-bind!-id ,id-stx ,self-expr ,phase ,phase-shift-id ',binding-sym))))

;; To support namespace-relative binding, bundle scope information for
;; the current namespace into a syntax object
(define (compile-namespace-scopes phase cctx)
  (define v (encode-namespace-scopes (compile-context-namespace cctx)))
  (compile-quote-syntax v phase cctx))

;; ----------------------------------------

;; Handle the `define-syntaxes`-with-zero-results hack for the top level
(define (generate-top-level-define-syntaxes gen-syms rhs transformer-set!s)
  `(call-with-values
    (lambda () ,rhs)
    (case-lambda
      [,gen-syms
       ,@transformer-set!s
       (void)]
      [() (void)]
      [args
       ;; Provoke the wrong-number-of-arguments error:
       (let-values ([,gen-syms (apply values args)])
         (void))])))
