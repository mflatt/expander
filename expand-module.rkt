#lang racket/base
(require racket/promise
         "syntax.rkt"
         "scope.rkt"
         "match.rkt"
         "phase.rkt"
         "namespace.rkt"
         "binding.rkt"
         "require+provide.rkt"
         "module-path.rkt"
         "lift-context.rkt"
         "core.rkt"
         "expand-context.rkt"
         "expand.rkt"
         "expand-require.rkt"
         "expand-provide.rkt"
         "compile.rkt"
         "cross-phase.rkt"
         "debug.rkt")

(add-core-form!
 'module
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (error "allowed only at the top level:" s))
   (expand-module s ctx #f)))

(add-core-form!
 'module*
 (lambda (s ctx)
   (error "illegal use (not in a module top-level):" s)))

(add-core-form!
 '#%module-begin
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'module-begin)
     (error "not in a module-definition context:" s))
   ;; This `#%module-begin` must be in a `module`; the
   ;; `module-begin-k` function continues that module's
   ;; expansion
   ((expand-context-module-begin-k ctx)
    s
    (struct-copy expand-context ctx
                 [module-begin-k #f]))))

(add-core-form!
 '#%declare
 (lambda (s ctx)
   ;; The `#%module-begin` expander handles `#%declare`
   (error "not in module body:" s)))

;; ----------------------------------------

(define (expand-module s ctx enclosing-self
                       #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                       #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f])
  (define m (match-syntax s '(module id:module-name initial-require body ...)))
   
   (define initial-require (syntax->datum (m 'initial-require)))
   (unless (or keep-enclosing-scope-at-phase
               (module-path? initial-require))
     (error "not a module path:" (m 'initial-require)))
   
   (define outside-scope (new-scope 'module))
   (define inside-scope (new-multi-scope))
   (define new-module-scopes (append (list inside-scope outside-scope)
                                     (if keep-enclosing-scope-at-phase
                                         (expand-context-module-scopes ctx)
                                         null)))

   (define self (make-self-module-path-index (syntax-e (m 'id:module-name))
                                             enclosing-self))
   (define m-ns (make-module-namespace (expand-context-namespace ctx)
                                       (module-path-index-resolve self)
                                       (and enclosing-self #t)))
   
   (define apply-module-scopes
     (make-apply-module-scopes outside-scope inside-scope 
                               ctx keep-enclosing-scope-at-phase
                               self enclosing-self))

   ;; Add the module's scope to the bodies
   (define bodys (map apply-module-scopes (m 'body)))
   
   ;; To keep track of all requires and provides
   (define requires+provides (make-requires+provides self))

   ;; Table of symbol picked for each binding in this module:
   (define defined-syms (make-hasheqv)) ; phase -> sym ->id

   ;; For variable repeferences before corresponding binding (phase >= 1)
   (define need-eventually-defined (make-hasheqv)) ; phase -> list of id

   ;; Initial require
   (define initial-require-s (apply-module-scopes (m 'initial-require)))
   (cond
    [(not keep-enclosing-scope-at-phase)
     ;; Install the initial require
     (perform-initial-require! initial-require self
                               initial-require-s
                               m-ns
                               requires+provides)]
    [else
     ;; For `(module* name #f ....)`, just register the enclosing module
     ;; as an import and visit it
     (add-required-module! requires+provides
                           enclosing-self
                           keep-enclosing-scope-at-phase
                           enclosing-is-cross-phase-persistent?)
     (namespace-module-visit! m-ns (module-path-index-resolve enclosing-self)
                              keep-enclosing-scope-at-phase)])
   
   ;; All module bodies start at phase 0
   (define phase 0)
   
   ;; The primitive `#%module-body` form calls this function to expand the
   ;; current module's body
   (define (module-begin-k mb ctx)
     (define mb-m (match-syntax mb '(#%module-begin body ...)))
     
     ;; In case the module body is expanded multiple times, we clear
     ;; the set of provides each time (but we accumulate requires, since those
     ;; may have been used for earlier expansions)
     (reset-provides! requires+provides)
     
     ;; In case `#%module-begin` expansion is forced on syntax that
     ;; that wasn't already introduced into the mdoule's inside scope,
     ;; add it to all the given body forms
     (define bodys
       (for/list ([body (in-list (mb-m 'body))])
         (add-scope body inside-scope)))
     
     ;; Accumulate `#%declare` content
     (define declared-keywords (make-hasheq))
     
     ;; The expansion of the module body happens in 4 passes:
     ;;  Pass 1: Partial expansion to determine imports and definitions
     ;;  Pass 2: Complete expansion of remaining expressions
     ;;  Pass 3: Parsing of provide forms
     ;;  Pass 4: Parsing of `module*` submodules
     
     ;; Passes 1 and 2 are nested via `begin-for-syntax`:
     (define expression-expanded-bodys
       (let phase-1-and-2-loop ([bodys bodys] [phase phase])

         ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         ;; Pass 1: partially expand to discover all bindings and install all 
         ;; defined macro transformers
         
         (define frame-id (gensym))
         (define partial-body-ctx (struct-copy expand-context ctx
                                               [context 'module]
                                               [phase phase]
                                               [namespace (namespace->namespace-at-phase-level m-ns phase)]
                                               [only-immediate? #t]
                                               [post-expansion-scope inside-scope]
                                               [module-scopes new-module-scopes]
                                               [all-scopes-stx initial-require-s]
                                               [frame-id frame-id]
                                               [need-eventually-defined (and (phase . >= . 1)
                                                                             need-eventually-defined)]
                                               [lifts (make-lift-context
                                                       (make-wrap-as-definition self frame-id
                                                                                inside-scope new-module-scopes
                                                                                defined-syms requires+provides))]
                                               [module-lifts (make-module-lift-context #t)]
                                               [lifts-to-module
                                                (make-lift-to-module-context
                                                 (make-parse-lifted-require m-ns self requires+provides)
                                                 #:end-as-expressions? #f)]))

         (define partially-expanded-bodys
           (partially-expand-bodys bodys
                                   #:original s
                                   #:tail? (zero? phase)
                                   #:phase phase
                                   #:ctx partial-body-ctx
                                   #:namespace m-ns
                                   #:self self
                                   #:frame-id frame-id
                                   #:requires-and-provides requires+provides
                                   #:need-eventually-defined need-eventually-defined
                                   #:module-scopes new-module-scopes
                                   #:defined-syms defined-syms
                                   #:declared-keywords declared-keywords
                                   #:loop phase-1-and-2-loop))

         ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         ;; Pass 2: finish expanding expressions
         
         (define body-ctx (struct-copy expand-context partial-body-ctx
                                       [only-immediate? #f]
                                       [post-expansion-scope #f]
                                       [lifts-to-module
                                        (make-lift-to-module-context
                                         (make-parse-lifted-require m-ns self requires+provides)
                                         #:end-as-expressions? #t)]))
         
         (finish-expanding-body-expressons partially-expanded-bodys
                                           #:tail? (zero? phase)
                                           #:phase phase
                                           #:ctx body-ctx
                                           #:self self)))

     ;; Check that any tentatively allowed reference at phase >= 1 is ok
     (check-defined-by-now need-eventually-defined self)
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Pass 3: resolve provides at all phases
     
     (define fully-expanded-bodys-except-post-submodules
       (resolve-provides expression-expanded-bodys
                         #:original s
                         #:requires-and-provides requires+provides
                         #:phase phase
                         #:self self
                         #:ctx (struct-copy expand-context ctx
                                            [requires+provides requires+provides])))

     ;; Validate any cross-phase persistence request
     (define is-cross-phase-persistent? (hash-ref declared-keywords '#:cross-phase-persistent #f))
     (when is-cross-phase-persistent?
       (unless (requires+provides-can-cross-phase-persistent? requires+provides)
         (error "cannot be cross-phase persistent due to required modules"
                (hash-ref declared-keywords '#:cross-phase-persistent)))
       (check-cross-phase-persistent-form fully-expanded-bodys-except-post-submodules))

     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Pass 4: expand `module*` submodules

     (define submod-ctx (struct-copy expand-context ctx
                                     [namespace m-ns]
                                     [module-scopes new-module-scopes]))
     
     (define declare-enclosing-module
       ;; Ensure this module on demand for `module*` submodules that might use it
       (delay (declare-module-for-expansion fully-expanded-bodys-except-post-submodules
                                            #:module-match m
                                            #:module-begin-match mb-m
                                            #:requires-and-provides requires+provides
                                            #:namespace m-ns
                                            #:self self
                                            #:enclosing enclosing-self)))
     
     (define fully-expanded-bodys
       (expand-post-submodules fully-expanded-bodys-except-post-submodules
                               #:declare-enclosing declare-enclosing-module
                               #:original s
                               #:phase phase
                               #:self self
                               #:enclosing-is-cross-phase-persistent? is-cross-phase-persistent?
                               #:ctx submod-ctx))
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Finish
     
     ;; Assemble the `#%module-begin` result:
     (rebuild
      mb
      `(,(mb-m '#%module-begin) ,@fully-expanded-bodys)))

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;; Actually expand the `#%module-body` form
   
   ;; The preceding function performs the expansion; here's where we
   ;; trigger it
      
   ;; Add `#%module-begin` around the body if it's not already present
   (define mb
     (ensure-module-begin bodys 
                          #:inside-scope inside-scope
                          #:new-module-scopes new-module-scopes
                          #:initial-require-s initial-require-s
                          #:m-ns m-ns
                          #:ctx ctx 
                          #:phase phase
                          #:s s))
   
   ;; Expand the body
   (define expanded-mb
     (expand mb
             (struct-copy expand-context ctx
                          [context 'module-begin]
                          [namespace m-ns]
                          [phase phase]
                          [module-scopes new-module-scopes]
                          [module-begin-k module-begin-k]
                          [use-site-scopes (box null)])))

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;; Assemble the `module` result
   
   (define generic-self (make-generic-self-module-path-index self))

   (attach-require-provide-properties
    requires+provides
    (syntax-module-path-index-shift
     (rebuild
      s
      `(,(m 'module) ,(m 'id:module-name) ,initial-require-s ,expanded-mb))
     self
     generic-self)
    self
    generic-self))

;; ----------------------------------------

;; Add `#%module-begin` to `bodys`, if needed
(define (ensure-module-begin bodys
                             #:inside-scope inside-scope
                             #:new-module-scopes new-module-scopes
                             #:initial-require-s initial-require-s
                             #:m-ns m-ns
                             #:ctx ctx 
                             #:phase phase
                             #:s s)
  (cond
   [(= 1 (length bodys))
    ;; Maybe it's already a `#%module-begin` form, or maybe it
    ;; will expand to one
    (cond
     [(eq? '#%module-begin (core-form-sym (car bodys) phase))
      ;; Done
      (car bodys)]
     [else
      ;; A single body form might be a macro that expands to
      ;; the primitive `#%module-begin` form:
      (define partly-expanded-body
        (expand (car bodys) (struct-copy expand-context ctx
                                         [context 'module-begin]
                                         [namespace m-ns]
                                         [phase phase]
                                         [only-immediate? #t]
                                         [post-expansion-scope inside-scope]
                                         [all-scopes-stx initial-require-s]
                                         [module-scopes new-module-scopes])))
      (cond
       [(eq? '#%module-begin (core-form-sym partly-expanded-body phase))
        ;; Yes, it expanded to `#%module-begin`
        partly-expanded-body]
       [else
        ;; No, it didn't expand to `#%module-begin`
        (add-module-begin (list partly-expanded-body) s initial-require-s phase)])])]
   [else
    ;; Multiple body forms definitely need a `#%module-begin` wrapper
    (add-module-begin bodys s initial-require-s phase)]))

;; Add `#%module-begin`, because it's needed
(define (add-module-begin bodys s initial-require-s phase)
  (define mb-id (datum->syntax initial-require-s '#%module-begin))
  ;; If `mb-id` is not bound, we'd like to give a clear error message
  (unless (resolve mb-id phase)
    (error "no #%module-begin binding in the module's language" s))
  (rebuild
   s
   `(,mb-id ,@bodys)))

;; ----------------------------------------

;; Make function to adjust syntax that appears in the original module body
(define (make-apply-module-scopes inside-scope outside-scope
                                  ctx keep-enclosing-scope-at-phase
                                  self enclosing-self)
  (lambda (s)
    (define s-without-enclosing
      (if keep-enclosing-scope-at-phase
          ;; Keep enclosing module scopes for `(module* _ #f ....)`
          s
          ;; Remove the scopes of the top level or a module outside of
          ;; this module, as well as any relevant use-site scopes
          (remove-use-site-scopes
           (for/fold ([s s]) ([sc (in-list (expand-context-module-scopes ctx))])
             (remove-scope s sc))
           ctx)))
    ;; Add outside- and inside-edge scopes
    (define s-with-edges
      (add-scope (add-scope s-without-enclosing
                            outside-scope)
                 inside-scope))
    (cond
     [keep-enclosing-scope-at-phase
      ;; Shift any references to the enclosing module to be relative to the
      ;; submodule
      (syntax-module-path-index-shift
       s-with-edges
       enclosing-self
       (module-path-index-join '(submod "..") self))]
     [else s-with-edges])))

;; ----------------------------------------

;; Pass 1 of `module` expansion, which uncovers definitions,
;; requires, and `module` submodules
(define (partially-expand-bodys bodys
                                #:original s
                                #:tail? tail?
                                #:phase phase
                                #:ctx partial-body-ctx
                                #:namespace m-ns
                                #:self self
                                #:frame-id frame-id
                                #:requires-and-provides requires+provides
                                #:need-eventually-defined need-eventually-defined
                                #:module-scopes module-scopes
                                #:defined-syms defined-syms
                                #:declared-keywords declared-keywords
                                #:loop phase-1-and-2-loop)
  (let loop ([tail? tail?] [bodys bodys])
    (cond
     [(null? bodys)
      (cond
       [tail?
        ;; Were at the very end of the module; if there are any lifted-to-end
        ;; declarations, keep going
        (define bodys
          (get-and-clear-ends! (expand-context-lifts-to-module partial-body-ctx)))
        (if (null? bodys)
            null
            (loop #t (add-post-expansion-scope bodys partial-body-ctx)))]
       [else null])]
     [else
      (define exp-body (expand (car bodys) partial-body-ctx))
      (append
       ;; Save any expressions lifted during partial expansion
       (get-and-clear-lifts! (expand-context-lifts partial-body-ctx))
       ;; Ditto for requires and provides
       (get-and-clear-requires-and-provides! (expand-context-lifts-to-module partial-body-ctx))
       ;; Ditto for modules, which need to be processed
       (loop #f (get-and-clear-module-lifts! (expand-context-module-lifts partial-body-ctx)))
       ;; Dispatch on form revealed by partial expansion
       (case (core-form-sym exp-body phase)
         [(begin)
          (define m (match-syntax exp-body '(begin e ...)))
          (loop tail? (append (m 'e) (cdr bodys)))]
         [(begin-for-syntax)
          (define m (match-syntax exp-body '(begin-for-syntax e ...)))
          (define nested-bodys (phase-1-and-2-loop (m 'e) (add1 phase)))
          (eval-nested-bodys nested-bodys (add1 phase) m-ns self)
          (cons
           (rebuild
            s
            `(,(m 'begin-for-syntax) ,@nested-bodys))
           (loop tail? (cdr bodys)))]
         [(define-values)
          (define m (match-syntax exp-body '(define-values (id ...) rhs)))
          (define ids (remove-use-site-scopes (m 'id) partial-body-ctx))
          (check-ids-unbound ids phase requires+provides #:in exp-body)
          (define syms (select-defined-syms-and-bind ids defined-syms self phase frame-id
                                                     module-scopes
                                                     requires+provides))
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [(define-syntaxes)
          (define m (match-syntax exp-body '(define-syntaxes (id ...) rhs)))
          (define ids (remove-use-site-scopes (m 'id) partial-body-ctx))
          (check-ids-unbound ids phase requires+provides #:in exp-body)
          (define syms (select-defined-syms-and-bind ids defined-syms self phase frame-id
                                                     module-scopes
                                                     requires+provides))
          ;; Expand and evaluate RHS:
          (define-values (exp-rhs vals)
            (expand+eval-for-syntaxes-binding (m 'rhs) ids
                                              (struct-copy expand-context partial-body-ctx
                                                           [need-eventually-defined need-eventually-defined])
                                              #:compile-time-for-self self))
          ;; Install transformers in the namespace for expansion:
          (for ([sym (in-list syms)]
                [val (in-list vals)]
                [id (in-list ids)])
            (maybe-install-free=id! val id phase)
            (namespace-set-transformer! m-ns phase sym val))
          (cons (rebuild exp-body
                         `(,(m 'define-syntaxes) ,ids ,exp-rhs))
                (loop tail? (cdr bodys)))]
         [(#%require)
          (define ready-body (remove-use-site-scopes exp-body partial-body-ctx))
          (define m (match-syntax ready-body '(#%require req ...)))
          (parse-and-perform-requires! (m 'req) self
                                       m-ns phase
                                       requires+provides)
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [(#%provide)
          ;; save for last pass
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [(module)
          ;; Submodule to parse immediately
          (define ready-body (remove-use-site-scopes exp-body partial-body-ctx))
          (define submod
            (expand-submodule ready-body self partial-body-ctx))
          (cons submod
                (loop tail? (cdr bodys)))]
         [(module*)
          ;; Submodule to save for after this module
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [(#%declare)
          (define m (match-syntax exp-body '(#%declare kw ...)))
          (for ([kw (in-list (m 'kw))])
            (unless (keyword? (syntax-e kw))
              (error "expected a keyword in `#%declare`:" kw))
            (unless (memq (syntax-e kw) '(#:cross-phase-persistent #:empty-namespace))
              (error "not an allowed declaration keyword:" kw))
            (when (hash-ref declared-keywords (syntax-e kw) #f)
              (error "keyword declared multiple times:" kw))
            (hash-set! declared-keywords (syntax-e kw) kw))
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [else
          ;; save expression for next pass
          (cons exp-body
                (loop tail? (cdr bodys)))]))])))

;; Convert lifted identifiers plus expression to a `define-values` form:
(define (make-wrap-as-definition self frame-id
                                 inside-scope module-scopes
                                 defined-syms requires+provides)
  (lambda (ids rhs phase)
    (define scoped-ids (for/list ([id (in-list ids)])
                         (add-scope id inside-scope)))
    (select-defined-syms-and-bind scoped-ids defined-syms self phase frame-id
                                  module-scopes
                                  requires+provides)
    (values scoped-ids
            (add-scope (datum->syntax
                        #f
                        (list (datum->syntax (syntax-shift-phase-level core-stx phase)
                                             'define-values)
                              ids
                              rhs))
                       inside-scope))))

(define (add-post-expansion-scope bodys ctx)
  (define sc (expand-context-post-expansion-scope ctx))
  (for/list ([body (in-list bodys)])
    (add-scope body sc)))

;; ----------------------------------------

;; Pass 2 of `module` expansion, which expands all expressions
(define (finish-expanding-body-expressons partially-expanded-bodys
                                          #:tail? tail?
                                          #:phase phase
                                          #:ctx body-ctx
                                          #:self self)
  (let loop ([tail? tail?] [bodys partially-expanded-bodys])
    (cond
     [(null? bodys)
      (cond
       [tail? 
        ;; Were at the very end of the module, again, so check for lifted-to-end
        ;; declarations
        (define bodys
          (get-and-clear-ends! (expand-context-lifts-to-module body-ctx)))
        (if (null? bodys)
            null
            (loop #t bodys))]
       [else bodys])]
     [else
      (case (core-form-sym (car bodys) phase)
        [(define-values)
         (define m (match-syntax (car bodys) '(define-values (id ...) rhs)))
         (define exp-rhs (expand (m 'rhs) (as-named-context (as-expression-context body-ctx)
                                                            (m 'id))))
         (cons (rebuild (car bodys)
                        `(,(m 'define-values) ,(m 'id) ,exp-rhs))
               (loop tail? (cdr bodys)))]
        [(define-syntaxes #%require #%provide begin-for-syntax module module* #%declare)
         (cons (car bodys)
               (loop tail? (cdr bodys)))]
        [else
         (define body (expand (car bodys) (as-expression-context body-ctx)))
         (define lifts
           ;; If there were any lifts, the right-hand sides need to be expanded
           (loop #f (get-and-clear-lifts! (expand-context-lifts body-ctx))))
         (define lifted-requires-and-provides
           ;; Get any requires and provides, keeping them as-is
           (get-and-clear-requires-and-provides! (expand-context-lifts-to-module body-ctx)))
         (define lifted-modules
           ;; If there were any module lifts, the `module` forms need to
           ;; be expanded
           (expand-non-module*-submodules (get-and-clear-module-lifts!
                                           (expand-context-module-lifts body-ctx))
                                          phase
                                          self
                                          body-ctx))
         (append
          lifts
          lifted-requires-and-provides
          lifted-modules
          (cons body
                (loop tail? (cdr bodys))))])])))

(define (check-defined-by-now need-eventually-defined self)
  ;; If `need-eventually-defined` is not empty, report an error
  (for ([(phase l) (in-hash need-eventually-defined)])
    (for ([id (in-list l)])
      (define b (resolve id phase))
      (unless (and b
                   (module-binding? b)
                   (eq? (module-binding-sym b) (syntax-e id))
                   (equal? (module-binding-module b) self))
        (error "reference to an unbound identifier:" id)))))

;; ----------------------------------------

;; Pass 3 of `module` expansion, which parses `provide` forms and
;; matches them up with defintiions and requires
(define (resolve-provides expression-expanded-bodys
                          #:original s
                          #:requires-and-provides requires+provides
                          #:phase phase
                          #:self self
                          #:ctx ctx)
  (let loop ([bodys expression-expanded-bodys] [phase phase])
    (cond
     [(null? bodys) null]
     [else
      (case (core-form-sym (car bodys) phase)
        [(#%provide)
         (define m (match-syntax (car bodys) '(#%provide spec ...)))
         (define specs
           (parse-and-expand-provides! (m 'spec)
                                       requires+provides self
                                       phase ctx
                                       expand rebuild))
         (cons (rebuild (car bodys)
                        `(,(m '#%provide) ,@specs))
               (loop (cdr bodys) phase))]
        [(begin-for-syntax)
         (define m (match-syntax (car bodys) '(begin-for-syntax e ...)))
         (define nested-bodys (loop (m 'e) (add1 phase)))
         (cons (rebuild s `(,(m 'begin-for-syntax) ,@nested-bodys))
               (loop (cdr bodys) phase))]
        [else
         (cons (car bodys)
               (loop (cdr bodys) phase))])])))

;; ----------------------------------------

;; In support of pass 4, declare a module (in a temporary namespace)
;; before any `module*` submodule is expanded
(define (declare-module-for-expansion fully-expanded-bodys-except-post-submodules
                                      #:module-match m
                                      #:module-begin-match mb-m
                                      #:requires-and-provides requires+provides
                                      #:namespace m-ns
                                      #:self self
                                      #:enclosing enclosing-self)
  (define tmp-mod (attach-require-provide-properties
                   requires+provides
                   (datum->syntax
                    #f
                    `(,(datum->syntax core-stx 'module) ,(m 'id:module-name) ,(m 'initial-require)
                      (,(mb-m '#%module-begin)
                       ,@fully-expanded-bodys-except-post-submodules)))
                   self
                   self))
  
  
  (define root-module-name (resolved-module-path-root-name
                            (module-path-index-resolve self)))
  (parameterize ([current-namespace m-ns]
                 [current-module-declare-name (make-resolved-module-path root-module-name)])
    (run-time-eval (compile-module tmp-mod
                                   (make-compile-context #:namespace m-ns
                                                         #:self enclosing-self
                                                         #:root-module-name root-module-name)
                                   #:self self
                                   #:as-submodule? #t))))

;; ----------------------------------------

;; Pass 4 of `module` expansion, which expands `module*` forms;
;; this pass muct happen after everything else for the module, since a
;; `module*` submodule can require from its enclosing module
(define (expand-post-submodules fully-expanded-bodys-except-post-submodules
                                #:declare-enclosing declare-enclosing-module
                                #:original s
                                #:phase phase
                                #:self self
                                #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                                #:ctx submod-ctx)
  (let loop ([bodys fully-expanded-bodys-except-post-submodules] [phase phase])
    (cond
     [(null? bodys) null]
     [else
      (case (core-form-sym (car bodys) phase)
        [(module*)
         ;; Ensure that the enclosing module is declared:
         (force declare-enclosing-module)
         (define submod
           (cond
            [(try-match-syntax (car bodys) '(module* name #f . _))
             ;; Need to shift the submodule relative to the enclosing module:
             (define neg-phase (phase- 0 phase))
             (define shifted-s (syntax-shift-phase-level (car bodys) neg-phase))
             (define submod
               (expand-submodule shifted-s self submod-ctx
                                 #:keep-enclosing-scope-at-phase neg-phase
                                 #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?))
             (syntax-shift-phase-level submod phase)]
            [else
             (expand-submodule (car bodys) self submod-ctx)]))
         (cons submod
               (loop (cdr bodys) phase))]
        [(begin-for-syntax)
         (define m (match-syntax (car bodys) '(begin-for-syntax e ...)))
         (define nested-bodys (loop (m 'e) (add1 phase)))
         (cons (rebuild s `(,(m 'begin-for-syntax) ,@nested-bodys))
               (loop (cdr bodys) phase))]
        [else
         (cons (car bodys)
               (loop (cdr bodys) phase))])])))

;; ----------------------------------------

(define (check-ids-unbound ids phase requires+provides #:in s)
  (for ([id (in-list ids)])
    (check-not-defined requires+provides id phase #:in s)))

(define (select-defined-syms-and-bind ids defined-syms self phase frame-id
                                      module-scopes
                                      requires+provides)
  (define defined-syms-at-phase
    (or (hash-ref defined-syms phase #f) (let ([ht (make-hasheq)])
                                           (hash-set! defined-syms phase ht)
                                           ht)))
  (for/list ([id (in-list ids)])
    (define sym (syntax-e id))
    (define defined-sym
      (if (and (not (hash-ref defined-syms-at-phase sym #f))
               ;; Only use `sym` directly if there are no
               ;; extra scopes on the binding form
               (no-extra-scopes? id module-scopes phase))
          sym
          (let loop ([pos 1])
            (define s (string->symbol (format "~a~a" sym pos)))
            (if (hash-ref defined-syms-at-phase s #f)
                (loop (add1 pos))
                s))))
    (hash-set! defined-syms-at-phase defined-sym id)
    (define b (make-module-binding self phase defined-sym #:frame-id frame-id))
    (remove-required-id! requires+provides id phase)
    (add-binding! id b phase)
    (add-defined-or-required-id! requires+provides id phase b)
    defined-sym))

(define (no-extra-scopes? id module-scopes phase)
  (bound-identifier=? id (add-scopes id module-scopes) phase))

;; ----------------------------------------

(define (eval-nested-bodys bodys phase m-ns self)
  ;; The defitions and expression bodys are fully expanded;
  ;; evaluate them
  (for ([body (in-list bodys)])
    (case (core-form-sym body phase)
      [(define-values)
       (define m (match-syntax body '(define-values (id ...) rhs)))
       (define ids (m 'id))
       (define vals (eval-for-bindings ids (m 'rhs) phase m-ns
                                       ;; In case we tentatively have to allow
                                       ;; a reference to a variable defined in
                                       ;; a later `begin-for-syntax`:
                                       #:compile-time-for-self self))
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
      [(#%provide #%require module module* begin-for-syntax #%declare)
       ;; handled earlier or later
       (void)]
      [else
       ;; an expression
       (expand-time-eval (compile-top body (make-compile-context
                                            #:namespace m-ns
                                            #:phase phase
                                            #:compile-time-for-self self)))])))

;; ----------------------------------------

(define (expand-submodule s self ctx
                          #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                          #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f])
  (define submod
    (expand-module s
                   (struct-copy expand-context ctx
                                [context 'module]
                                [only-immediate? #f]
                                [post-expansion-scope #f]
                                [phase 0])
                   self
                   #:keep-enclosing-scope-at-phase keep-enclosing-scope-at-phase
                   #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?))
  
  ;; Compile and declare the submodule for use by later forms
  ;; in the enclosing module:
  (define ns (expand-context-namespace ctx))
  (define root-module-name (resolved-module-path-root-name
                            (module-path-index-resolve self)))
  (parameterize ([current-namespace ns]
                 [current-module-declare-name (make-resolved-module-path root-module-name)])
    (run-time-eval (compile-module submod 
                                   (make-compile-context #:namespace ns
                                                         #:self self
                                                         #:root-module-name root-module-name)
                                   #:as-submodule? #t)))

  ;; Return the expanded submodule
  submod)

;; Expand `module` forms, leave `module*` forms alone:
(define (expand-non-module*-submodules bodys phase self ctx)
  (for/list ([body (in-list bodys)])
    (case (core-form-sym body phase)
      [(module)
       (expand-submodule body self ctx)]
      [else body])))

;; ----------------------------------------

(define (make-parse-lifted-require m-ns self requires+provides)
  (lambda (s phase)
    (define m (match-syntax s '(#%require req)))
    (parse-and-perform-requires! (list (m 'req)) self
                                 m-ns phase
                                 requires+provides)))
