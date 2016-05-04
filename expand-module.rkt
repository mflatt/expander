#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "namespace.rkt"
         "binding.rkt"
         "require.rkt"
         "module-path.rkt"
         "core.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "expand-require.rkt"
         "expand-provide.rkt"
         "compile.rkt")

(add-core-form!
 'module
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (error "allowed only at the top level:" s))
   
   (define m (match-syntax s '(module id:module-name initial-require body ...)))
   
   (define initial-require (syntax->datum (m 'initial-require)))
   (unless (module-path? initial-require)
     (error "not a module path:" (m 'initial-require)))
   
   (define outside-scope (new-scope))
   (define inside-scope (new-multi-scope))
   (define inside-stx (add-scope empty-syntax inside-scope))

   (define self 'self)
   (define m-ns (make-module-namespace (expand-context-namespace ctx)
                                       'self))
   
   (define enclosing-scopes
     (expand-context-module-scopes ctx))
   (define (apply-module-scopes s)
     (define s-without-enclosing
       (for/fold ([s s]) ([sc (in-list enclosing-scopes)])
         (remove-scope s sc)))
     (add-scope (add-scope s-without-enclosing
                           outside-scope)
                inside-scope))

   ;; To track requires and provides:
   (define require-provide (make-require-provide-registry))
   (define (add-defined-or-required-id! id phase binding)
     (register-defined-or-required-id! require-provide id phase binding))

   ;; Initial require:
   (perform-initial-require! initial-require
                             (apply-module-scopes (m 'initial-require))
                             m-ns
                             add-defined-or-required-id!)
   
   (define bodys (map apply-module-scopes (m 'body)))

   (define phase 0)
   
   ;; Phases 1 and 2 are nested via `begin-for-syntax`:
   (define expression-expanded-bodys
     (let phase-1-and-2-loop ([bodys bodys] [phase phase])

       ;; ------------------------------------------------------------
       ;; Pass 1: partially expand to discover all bindings and install all 
       ;; defined macro transformers
       
       (define partial-body-ctx (struct-copy expand-context ctx
                                             [context 'module]
                                             [phase phase]
                                             [namespace m-ns]
                                             [only-immediate? #t]
                                             [post-expansion-scope inside-scope]
                                             [module-scopes
                                              (list inside-scope outside-scope)]))
       
       ;; Symbol picked for each binding in this module:
       (define local-names (make-hasheq))

       (define partially-expanded-bodys
         (let loop ([bodys bodys] [done-bodys null])
           (cond
            [(null? bodys) (reverse done-bodys)]
            [else
             (define exp-body (expand (car bodys) partial-body-ctx))
             (case (core-form-sym exp-body phase)
               [(begin)
                (define m (match-syntax exp-body '(begin e ...)))
                (loop (append (m 'e) (cdr bodys))
                      done-bodys)]
               [(begin-for-syntax)
                (define m (match-syntax exp-body '(begin-for-syntax e ...)))
                (define nested-bodys (phase-1-and-2-loop (m 'e) (add1 phase)))
                (eval-nested-bodys nested-bodys (add1 phase) m-ns self)
                (loop (cdr bodys)
                      (cons
                       (rebuild
                        s
                        `(,(m 'begin-for-syntax) ,@nested-bodys))
                       done-bodys))]
               [(define-values)
                (define m (match-syntax exp-body '(define-values (id ...) rhs)))
                (define ids (m 'id))
                (check-ids-unbound ids phase)
                (define syms (select-local-names-and-bind ids local-names self phase
                                                          add-defined-or-required-id!))
                (loop (cdr bodys)
                      (cons (car bodys) done-bodys))]
               [(define-syntaxes)
                (define m (match-syntax exp-body '(define-syntaxes (id ...) rhs)))
                (define ids (m 'id))
                (check-ids-unbound ids phase)
                (define syms (select-local-names-and-bind ids local-names self phase
                                                          add-defined-or-required-id!))
                ;; Expand and evaluate RHS:
                (define-values (exp-rhs vals)
                  (expand+eval-for-syntaxes-binding (m 'rhs) ids partial-body-ctx))
                ;; Install transformers in the namespace for expansion:
                (for ([key (in-list syms)]
                      [val (in-list vals)])
                  (namespace-set-transformer! m-ns phase key val))
                (loop (cdr bodys)
                      (cons (rebuild (car bodys)
                                     `(,(m 'define-syntaxes) ,ids ,exp-rhs))
                            done-bodys))]
               [(#%require)
                (define m (match-syntax exp-body '(#%require req ...)))
                (parse-and-perform-requires! (m 'req) m-ns phase
                                             add-defined-or-required-id!)
                (loop (cdr bodys)
                      (cons (car bodys)
                            done-bodys))]
               [(#%provide)
                ;; save for last pass
                (loop (cdr bodys)
                      (cons (car bodys)
                            done-bodys))]
               [else
                ;; save expression for next pass
                (loop (cdr bodys)
                      (cons (car bodys)
                            done-bodys))])])))

       ;; ------------------------------------------------------------
       ;; Pass 2: finish expanding expressions
       
       (define body-ctx (struct-copy expand-context partial-body-ctx
                                     [only-immediate? #f]
                                     [post-expansion-scope #f]))
       
       (define expression-expanded-bodys
         (let loop ([bodys partially-expanded-bodys] [done-bodys null])
           (cond
            [(null? bodys) (reverse done-bodys)]
            [else
             (case (core-form-sym (car bodys) phase)
               [(define-values)
                (define m (match-syntax (car bodys) '(define-values (id ...) rhs)))
                (define exp-rhs (expand (m 'rhs) body-ctx))
                (loop (cdr bodys)
                      (cons (rebuild (car bodys)
                                     `(,(m 'define-values) ,(m 'id) ,exp-rhs))
                            done-bodys))]
               [(define-syntaxes #%require #%provide begin-for-syntax)
                (loop (cdr bodys)
                      (cons (car bodys)
                            done-bodys))]
               [else
                (loop (cdr bodys)
                      (cons (expand (car bodys) body-ctx)
                            done-bodys))])])))
       
       expression-expanded-bodys))

   ;; ------------------------------------------------------------
   ;; Pass 3: resolve provides at all phases
   
   (define fully-expanded-bodys
     (let loop ([bodys expression-expanded-bodys] [phase phase] [done-bodys null])
       (cond
        [(null? bodys) (reverse done-bodys)]
        [else
         (case (core-form-sym (car bodys) phase)
           [(#%provide)
            (define m (match-syntax (car bodys) '(#%provide spec ...)))
            (define specs
              (parse-and-expand-provides! (m 'spec)
                                          require-provide self
                                          phase ctx
                                          expand rebuild))
            (loop (cdr bodys)
                  phase
                  (cons (rebuild (car bodys)
                                 `(,(m '#%provide) ,@specs))
                        done-bodys))]
           [(begin-for-syntax)
            (define m (match-syntax (car bodys) '(begin-for-syntax e ...)))
            (define nested-bodys (loop (m 'e) (add1 phase) null))
            (loop (cdr bodys)
                  phase
                  (cons (rebuild s `(,(m 'begin-for-syntax) ,@nested-bodys))
                        done-bodys))]
           [else
            (loop (cdr bodys)
                  phase
                  (cons (car bodys)
                        done-bodys))])])))
           
   ;; ------------------------------------------------------------
   ;; Assemble the result

   (attach-require-provide-properties
    (rebuild
     s
     `(,(m 'module) ,(m 'id:module-name) ,(m 'initial-require) ,@fully-expanded-bodys))
    require-provide
    self)))

;; ----------------------------------------

(define (check-ids-unbound ids phase)
  (for ([id (in-list ids)])
    (when (resolve id phase #:exactly? #t)
      (error "identifier is already defined or required:" id))))

(define (select-local-names-and-bind ids local-names self phase
                                     add-defined-or-required-id!)
  (for/list ([id (in-list ids)])
    (define sym (syntax-e id))
    (define local-sym
      (if (not (hash-ref local-names sym #f))
          sym
          (let loop ([pos 1])
            (define s (string->symbol (format "~a~a" sym pos)))
            (if (hash-ref local-names s #f)
                (loop (add1 pos))
                s))))
    (hash-set! local-names local-sym id)
    (define b (module-binding self phase local-sym
                              self phase local-sym
                              0))
    (add-binding! id b phase)
    (add-defined-or-required-id! id phase b)
    local-sym))

;; ----------------------------------------

(define (eval-nested-bodys bodys phase m-ns self)
  ;; The defitions and expression bodys are fully expanded;
  ;; evaluate them
  (for ([body (in-list bodys)])
    (case (core-form-sym body phase)
      [(define-values)
       (define m (match-syntax body '(define-values (id ...) rhs)))
       (define ids (m 'id))
       (define vals (eval-for-bindings ids (m 'rhs) phase m-ns))
       (for ([id (in-list ids)]
             [val (in-list vals)])
         (define b (resolve id phase))
         (unless (and (module-binding? b)
                      (equal? self (module-binding-module b)))
           (error "internal error: nested binding is not to self"))
         (namespace-set-variable! m-ns phase (module-binding-sym b) val))]
      [(define-syntaxes)
       ;; already evaluated during expandion
       (void)]
      [(#f)
       ;; an expression
       (expand-time-eval `(#%expression ,(compile (car bodys) phase m-ns)))]
      [else
       ;; other forms handled earlier or later
       (void)])))
