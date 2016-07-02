#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "../common/phase.rkt"
         "../namespace/core.rkt"
         "../namespace/namespace.rkt"
         "../expand/root-expand-context.rkt"
         "../common/module-path.rkt"
         "module-use.rkt"
         "serialize.rkt"
         "built-in-symbol.rkt"
         "../host/linklet.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "id-to-symbol.rkt"
         "instance.rkt"
         "namespace-scope.rkt"
         "expr.rkt")

(provide compile-forms

         compile-namespace-scopes)

;; Compiles a module body or sequence of top-level forms, returning a
;; linklet directory to cover all phases covered by the forms
(define (compile-forms bodys cctx mpis
                       #:encoded-root-expand-ctx-box [encoded-root-expand-ctx-box #f] ; encoded root context, if any
                       #:root-ctx-only-if-syntax? [root-ctx-only-if-syntax? #f]
                       #:compiled-expression-callback [compiled-expression-callback void]
                       #:other-form-callback [other-form-callback void]
                       #:to-source? [to-source? #f])
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
  
  ;; Keep track of whether any `define-syntaxes` appeared at any phase
  (define saw-define-syntaxes? #f)
  
  (when (compile-context-module-self cctx)
    ;; In a module, select non-conflicting symbols for definitions,
    ;; first, in the hope that we can just the names as-is; and we'll
    ;; rename locals as needed to avoid these names
    (let loop! ([bodys bodys] [phase phase] [header (find-or-create-header! phase)])
      (for ([body (in-list bodys)])
        (case (core-form-sym body phase)
          [(define-values)
           (define-match m body '(define-values (id ...) rhs))
           (for ([sym (in-list (def-ids-to-binding-syms (m 'id) phase self))])
             (define def-sym (select-fresh sym header))
             (hash-set! (header-binding-sym-to-define-sym header)
                        sym
                        def-sym)
             (set-header-binding-syms-in-order! header
                                                (cons sym
                                                      (header-binding-syms-in-order header)))
             (register-as-defined! header def-sym))]
          [(begin-for-syntax)
           (define-match m body '(begin-for-syntax e ...))
           (loop! (m 'e) (add1 phase) (find-or-create-header! (add1 phase)))]))))
  
  ;; Provided for callbacks to detect reuiqred references:
  (define ((as-required? header) sym)
    (registered-as-required? header sym))

  ;; Compile each form in `bodys`, recording results in `phase-to-body`
  (define last-i (sub1 (length bodys)))
  (let loop! ([bodys bodys] [phase phase] [header (find-or-create-header! phase)])
    (for ([in-body (in-list bodys)]
          [i (in-naturals)])
      (define body (syntax-disarm in-body))
      (case (core-form-sym body phase)
        [(define-values)
         (define-match m body '(define-values (id ...) rhs))
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
                                                binding-sym
                                                #f
                                                #:defined? #t))]))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context cctx
                                           [phase phase]
                                           [header header])))
         (compiled-expression-callback rhs (length def-syms) phase (as-required? header))
         ;; Generate a definition:
         (add-body! phase `(define-values ,def-syms ,rhs))
         (unless (or (compile-context-module-self cctx)
                     (null? ids))
           ;; Not in a module; ensure that the defined names are
           ;; treated as mutable
           (add-body! phase
                      `(if #f
                        (begin
                          ,@(for/list ([def-sym (in-list def-syms)])
                              `(set! ,def-sym #f)))
                        (void)))
           ;; Also, install a binding at run time
           (add-body! phase (compile-top-level-bind
                             ids binding-syms
                             (struct-copy compile-context cctx
                                          [phase phase]
                                          [header header])
                             #f)))]
        [(define-syntaxes)
         (define-match m body '(define-syntaxes (id ...) rhs))
         (define ids (m 'id))
         (define binding-syms (def-ids-to-binding-syms ids phase self))
         (define next-header (find-or-create-header! (add1 phase)))
         (define gen-syms (for/list ([binding-sym (in-list binding-syms)])
                            (select-fresh binding-sym next-header)))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context cctx
                                           [phase (add1 phase)]
                                           [header next-header])))
         (compiled-expression-callback rhs (length gen-syms) (add1 phase) (as-required? header))
         (define transformer-set!s (for/list ([binding-sym (in-list binding-syms)]
                                              [gen-sym (in-list gen-syms)])
                                     `(,set-transformer!-id ',binding-sym ,gen-sym)))
         (cond
          [(compile-context-module-self cctx)
           (add-body! (add1 phase) `(let-values ([,gen-syms ,rhs])
                                     (begin
                                       ,@transformer-set!s
                                       (void))))]
          [else
           (add-body! (add1 phase)
                      (generate-top-level-define-syntaxes
                       gen-syms rhs transformer-set!s
                       (compile-top-level-bind
                        ids binding-syms
                        (struct-copy compile-context cctx
                                     [phase phase]
                                     [header header])
                        gen-syms)))])
         (set! saw-define-syntaxes? #t)]
        [(begin-for-syntax)
         (define-match m body '(begin-for-syntax e ...))
         (loop! (m 'e) (add1 phase) (find-or-create-header! (add1 phase)))]
        [(#%require #%provide #%declare module module*)
         ;; Must be handled separately, if allowed at all
         (define e (other-form-callback body (struct-copy compile-context cctx
                                                          [phase phase]
                                                          [header header])))
         (when e
           (compiled-expression-callback e #f phase (as-required? header))
           (add-body! phase e))]
        [else
         (define e (compile body
                            (struct-copy compile-context cctx
                                         [phase phase]
                                         [header header])
                            (= i last-i)))
         (compiled-expression-callback e #f phase (as-required? header))
         (add-body! phase e)])))

  ;; Register root-expand-context, if any, encoded as a syntax object;
  ;; see also "../eval/root-context.rkt"
  (define encoded-root-expand-header
    (and encoded-root-expand-ctx-box
         (unbox encoded-root-expand-ctx-box) ; box => can be cleared by a callback
         (not (and root-ctx-only-if-syntax?
                   (not saw-define-syntaxes?)
                   (for/and ([h (in-hash-values phase-to-header)])
                     (header-empty-syntax-literals? h))))
         (let ([h (find-or-create-header! 'root-ctx)])
           (add-syntax-literal! h (unbox encoded-root-expand-ctx-box))
           h)))

  ;; Collect resulting phases
  (define phases-in-order (sort (hash-keys phase-to-body) <))
  (define min-phase (if (pair? phases-in-order)
                        (car phases-in-order)
                        phase))
  (define max-phase (if (pair? phases-in-order)
                        (car (reverse phases-in-order))
                        phase))

  ;; Compute linking info for each phase
  (struct link-info (link-module-uses imports extra-inspectorsss def-decls))
  (define phase-to-link-info
    (for/hash ([phase (in-list phases-in-order)])
      (define header (hash-ref phase-to-header phase #f))
      (define-values (link-module-uses imports extra-inspectorsss def-decls)
        (generate-links+imports header phase cctx))
      (values phase (link-info link-module-uses imports extra-inspectorsss def-decls))))
  
  ;; Generate the phase-specific linking units
  (define body-linklets
    (for/hasheq ([phase (in-list phases-in-order)])
      (define bodys (hash-ref phase-to-body phase))
      (define li (hash-ref phase-to-link-info phase))
      (define binding-sym-to-define-sym
        (header-binding-sym-to-define-sym (hash-ref phase-to-header phase)))
      (values
       phase
       ((if to-source? values compile-linklet)
        `(linklet
          ;; imports
          (,@(if (compile-context-module-self cctx)
                 `([(mpi-vector ,mpi-vector-id)]
                   [(syntax-literalss ,syntax-literalss-id)
                    (get-syntax-literal! ,get-syntax-literal!-id)])
                 `([(top-level-bind! ,top-level-bind!-id)
                    (top-level-require! ,top-level-require!-id)]
                   [(mpi-vector ,mpi-vector-id)
                    (syntax-literalss ,syntax-literalss-id)]))
           ,instance-imports
           ,@(link-info-imports li))
          ;; exports
          (,@(link-info-def-decls li)
           ,@(for/list ([binding-sym (in-list (header-binding-syms-in-order
                                               (hash-ref phase-to-header phase)))])
               (define def-sym (hash-ref binding-sym-to-define-sym binding-sym))
               `[,def-sym ,binding-sym]))
          ;; body
          ,@(reverse bodys))))))
  
  (define phase-to-link-module-uses
    (for/hash ([(phase li) (in-hash phase-to-link-info)])
      (values phase (link-info-link-module-uses li))))
  
  (define phase-to-link-module-uses-expr
    `(hasheqv ,@(apply
                 append
                 (for/list ([phase (in-list phases-in-order)])
                   (list phase `(list ,@(serialize-module-uses (hash-ref phase-to-link-module-uses phase)
                                                               mpis)))))))

  (define phase-to-link-extra-inspectorsss
    (for/hash ([(phase li) (in-hash phase-to-link-info)])
      (values phase (link-info-extra-inspectorsss li))))

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
          phase-to-link-extra-inspectorsss
          syntax-literalss
          (and encoded-root-expand-header
               (header-syntax-literals encoded-root-expand-header))))

;; ----------------------------------------

;; Evaluating a top-level definition has a secondary effect: it
;; adjusts the binding of defined identifiers. This mingling of
;; evaluation and expansion is the main weirdness of the top
;; level.
(define (compile-top-level-bind ids binding-syms cctx trans-exprs)
  (define phase (compile-context-phase cctx))
  (define self (compile-context-self cctx))
  (define header (compile-context-header cctx))
  (define mpis (header-module-path-indexes header))
  ;; The binding that we install at run time should not include
  ;; the temporary binding scope that the expander added:
  (define top-level-bind-scope (root-expand-context-top-level-bind-scope
                                (namespace-get-root-expand-ctx
                                 (compile-context-namespace cctx))))
  ;; For installing a binding:
  (define self-expr (add-module-path-index! mpis self))
  ;; Generate calls to `top-level-bind!`:
  `(begin
    ,@(for/list ([id (in-list ids)]
                 [binding-sym (in-list binding-syms)]
                 [trans-expr (in-list (or trans-exprs
                                          (for/list ([id (in-list ids)])
                                            `'#f)))])
        (define id-stx
          (compile-quote-syntax (remove-scope id top-level-bind-scope)
                                phase
                                cctx))
        `(,top-level-bind!-id ,id-stx ,self-expr ,phase ,phase-shift-id ,ns-id ',binding-sym ,trans-expr))))

;; To support namespace-relative binding, bundle scope information for
;; the current namespace into a syntax object
(define (compile-namespace-scopes phase cctx)
  (define v (encode-namespace-scopes (compile-context-namespace cctx)))
  (compile-quote-syntax v phase cctx))

;; ----------------------------------------

;; Handle the `define-syntaxes`-with-zero-results hack for the top level;
;; beware that we make two copies of `finish`
(define (generate-top-level-define-syntaxes gen-syms rhs transformer-set!s finish)
  `(call-with-values
    (lambda () ,rhs)
    (case-lambda
      [,gen-syms
       (begin
         ,@transformer-set!s
         ,finish
         (void))]
      [()
       (let-values ([,gen-syms (values ,@(for/list ([s (in-list gen-syms)]) `'#f))])
         (begin
           ,finish
           (void)))]
      [args
       ;; Provoke the wrong-number-of-arguments error:
       (let-values ([,gen-syms (apply values args)])
         (void))])))
