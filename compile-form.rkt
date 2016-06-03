#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "phase.rkt"
         "core.rkt"
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
         "compile-expr.rkt")

(provide compile-forms)

;; Compiles a module body or sequence of top-level forms, returning a
;; linklet directory to cover all phases covered by the forms
(define (compile-forms bodys cctx mpis
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
  
  ;; Select non-conflicting symbols for definitions, first, in the
  ;; hope that we can just the names as-is; and we'll rename locals as
  ;; needed to avoid these names
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
          (loop! (m 'e) (add1 phase) (find-or-create-header! (add1 phase)))])))

  ;; Compile each form in `bodys`, recording results in `phase-to-body`
  (let loop! ([bodys bodys] [phase 0] [header (find-or-create-header! 0)])
    (for ([body (in-list bodys)])
      (case (core-form-sym body phase)
        [(define-values)
         (define m (match-syntax body '(define-values (id ...) rhs)))
         (define binding-syms (def-ids-to-binding-syms (m 'id) phase self))
         (define def-syms (for/list ([binding-sym (in-list binding-syms)])
                            (hash-ref (header-binding-sym-to-define-sym header)
                                      binding-sym)))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context cctx
                                           [phase phase]
                                           [header header])))
         (compiled-expression-callback rhs (length def-syms) phase)
         (add-body! phase `(define-values ,def-syms ,rhs))]
        [(define-syntaxes)
         (define m (match-syntax body '(define-syntaxes (id ...) rhs)))
         (define binding-syms (def-ids-to-binding-syms (m 'id) phase self))
         (define next-header (find-or-create-header! (add1 phase)))
         (define gen-syms (for/list ([binding-sym (in-list binding-syms)])
                            (select-fresh binding-sym next-header)))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context cctx
                                           [phase (add1 phase)]
                                           [header next-header])))
         (compiled-expression-callback rhs (length gen-syms) (add1 phase))
         (add-body! (add1 phase) `(let-values ([,gen-syms ,rhs])
                                   ,@(for/list ([binding-sym (in-list binding-syms)]
                                                [gen-sym (in-list gen-syms)])
                                       `(,set-transformer!-id ',binding-sym ,gen-sym))
                                   (void)))]
        [(begin-for-syntax)
         (define m (match-syntax body `(begin-for-syntax e ...)))
         (loop! (m 'e) (add1 phase) (find-or-create-header! (add1 phase)))]
        [(#%require #%provide #%declare module module*)
         ;; Must be handled separately, if allowed at all
         (other-form-callback body phase)]
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
  (struct link-info (link-module-uses imports))
  (define phase-to-link-info
    (for/hash ([phase (in-list phases-in-order)])
      (define header (hash-ref phase-to-header phase))
      (define-values (link-module-uses imports def-decls)
        (generate-links+imports header phase cctx))
      (unless (null? def-decls)
        (error "internal error definition declarations for closed module"))
      (values phase (link-info link-module-uses imports))))

  ;; Generate the phase-specific linking units
  (define body-linklets
    (for/hash ([phase (in-list phases-in-order)])
      (define bodys (hash-ref phase-to-body phase))
      (define li (hash-ref phase-to-link-info phase))
      (define syntax-literals (header-syntax-literals (hash-ref phase-to-header phase)))
      (define binding-sym-to-define-sym
        (header-binding-sym-to-define-sym (hash-ref phase-to-header phase)))
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
          #:export (,@(for/list ([binding-sym (in-list (header-binding-syms-in-order
                                                        (hash-ref phase-to-header phase)))])
                        (define def-sym (hash-ref binding-sym-to-define-sym binding-sym))
                        `[,def-sym ,binding-sym]))
          ,@(generate-syntax-literals! syntax-literals mpis phase self)
          ,@(reverse bodys))))))
  
  (define phase-to-link-module-uses
    (for/hash ([(phase li) (in-hash phase-to-link-info)])
      (values phase (link-info-link-module-uses li))))
  
  (define phase-to-link-module-uses-expr
    `(hash ,@(apply
              append
              (for/list ([phase (in-list phases-in-order)])
                (list phase `(list ,@(serialize-module-uses (hash-ref phase-to-link-module-uses phase)
                                                            mpis)))))))
  
  (values body-linklets   ; main compilation result
          min-phase
          max-phase
          phase-to-link-module-uses
          phase-to-link-module-uses-expr))
