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

(provide compile-module)

;; Compiles module to a set of linklets that is returned as a linklet
;; directory
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
  
  (define phase-to-header (make-hasheqv)) ; phase -> header
  (define (find-or-create-header! phase)
    (or (hash-ref phase-to-header phase #f)
        (let ([header (make-header mpis)])
          (hash-set! phase-to-header phase header)
          header)))
  
  (define cross-phase-persistent? #f)
  
  ;; Select non-conflicting symbols for definitions, first, in the
  ;; hope that we can just the names as-is; and we'll rename locals as
  ;; needed to avoid these names
  (let loop! ([bodys bodys] [phase 0] [header (find-or-create-header! 0)])
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
                              (struct-copy compile-context body-cctx
                                           [phase phase]
                                           [header header])))
         (check-side-effects! rhs (length def-syms) phase)
         (add-body! phase `(define-values ,def-syms ,rhs))]
        [(define-syntaxes)
         (define m (match-syntax body '(define-syntaxes (id ...) rhs)))
         (define binding-syms (def-ids-to-binding-syms (m 'id) phase self))
         (define next-header (find-or-create-header! (add1 phase)))
         (define gen-syms (for/list ([binding-sym (in-list binding-syms)])
                            (select-fresh binding-sym next-header)))
         (define rhs (compile (m 'rhs)
                              (struct-copy compile-context body-cctx
                                           [phase (add1 phase)]
                                           [header next-header])))
         (check-side-effects! rhs (length gen-syms) (add1 phase))
         (add-body! (add1 phase) `(let-values ([,gen-syms ,rhs])
                                   ,@(for/list ([binding-sym (in-list binding-syms)]
                                                [gen-sym (in-list gen-syms)])
                                       `(,set-transformer!-id ',binding-sym ,gen-sym))
                                   (void)))]
        [(begin-for-syntax)
         (define m (match-syntax body `(begin-for-syntax e ...)))
         (loop! (m 'e) (add1 phase) (find-or-create-header! (add1 phase)))]
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
                                         [header header])))
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
      (define header (hash-ref phase-to-header phase))
      (define-values (link-module-uses imports def-decls)
        (generate-links+imports header phase body-cctx))
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

  (define max-header-phase
    (if (zero? (hash-count phase-to-header))
        0
        (apply max (hash-keys phase-to-header))))

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
         (make-vector ,(add1 max-header-phase) #f))
       ,@declaration-body)))

  (hash->linklet-directory
   (for/fold ([ht (hash-set body-linklets #"" declaration-linklet)])
             ([sm (in-list (append pre-submodules post-submodules))])
     (hash-set ht (encode-linklet-directory-key (car sm)) (cdr sm)))))

;; ----------------------------------------

