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

(define (expand-module s init-ctx enclosing-self
                       #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                       #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f]
                       #:enclosing-requires+provides [enclosing-r+p #f])
  (define m (match-syntax s '(module id:module-name initial-require body ...)))
   
   (define initial-require (syntax->datum (m 'initial-require)))
   (unless (or keep-enclosing-scope-at-phase
               (module-path? initial-require))
     (error "not a module path:" (m 'initial-require)))
   
   ;; All module bodies start at phase 0
   (define phase 0)
   
   (define outside-scope (new-scope 'module))
   (define inside-scope (new-multi-scope (syntax-e (m 'id:module-name))))

   (define self (make-self-module-path-index (syntax-e (m 'id:module-name))
                                             enclosing-self))
   
   (define enclosing-mod (and enclosing-self
                              (module-path-index-join '(submod "..") self)))
   
   (define apply-module-scopes
     (make-apply-module-scopes outside-scope inside-scope 
                               init-ctx keep-enclosing-scope-at-phase
                               self enclosing-self enclosing-mod))

   ;; Initial require name provides the module's base scopes
   (define initial-require-s (apply-module-scopes (m 'initial-require)))

   (define root-ctx (make-root-expand-context
                     #:initial-scopes (if keep-enclosing-scope-at-phase
                                          (root-expand-context-module-scopes init-ctx)
                                          null)
                     #:module-push-scope outside-scope
                     #:post-expansion-scope inside-scope
                     #:all-scopes-stx initial-require-s))
   
   ;; Extract combined scopes
   (define new-module-scopes (root-expand-context-module-scopes root-ctx))

   ;; A frame-id is used to determine when use-site scopes are needed
   (define frame-id (root-expand-context-frame-id root-ctx))

   ;; Make a namespace for module expansion
   (define m-ns (make-module-namespace (expand-context-namespace init-ctx)
                                       #:mpi self
                                       #:root-expand-context root-ctx
                                       #:for-submodule? (and enclosing-self #t)))
   
   ;; Initial context for all body expansions:
   (define ctx (struct-copy expand-context (copy-root-expand-context init-ctx root-ctx)
                            [namespace m-ns]
                            [phase phase]))
   
   ;; Add the module's scope to the bodies
   (define bodys (map apply-module-scopes (m 'body)))
   
   ;; To keep track of all requires and provides
   (define requires+provides (make-requires+provides self))

   ;; Table of symbols picked for each binding in this module:
   (define defined-syms (make-hasheqv)) ; phase -> sym ->id

   ;; Initial require
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
                           enclosing-mod
                           keep-enclosing-scope-at-phase
                           enclosing-is-cross-phase-persistent?)
     (add-enclosing-module-defined-and-required! requires+provides
                                                 #:enclosing-requires+provides enclosing-r+p
                                                 enclosing-mod
                                                 keep-enclosing-scope-at-phase)
     (namespace-module-visit! m-ns enclosing-self
                              keep-enclosing-scope-at-phase)])
   
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
     
     ;; For variable repeferences before corresponding binding (phase >= 1)
     (define need-eventually-defined (make-hasheqv)) ; phase -> list of id
     
     ;; For `syntax-local-lift-module-end-declaration`, which is accumulated
     ;; across phases:
     (define module-ends (make-shared-module-ends))
     
     ;; Accumulate `#%declare` content
     (define declared-keywords (make-hasheq))
     
     ;; Accumulated declared submodule names for `syntax-local-submodules`
     (define declared-submodule-names (make-hasheq))
     
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
         
         (define partial-body-ctx (struct-copy expand-context ctx
                                               [context 'module]
                                               [phase phase]
                                               [namespace (namespace->namespace-at-phase m-ns phase)]
                                               [only-immediate? #t]
                                               [need-eventually-defined (and (phase . >= . 1)
                                                                             need-eventually-defined)]
                                               [declared-submodule-names declared-submodule-names]
                                               [lifts (make-lift-context ; FIXME: share single instance for same phase?
                                                       (make-wrap-as-definition self frame-id
                                                                                inside-scope new-module-scopes
                                                                                defined-syms requires+provides))]
                                               [module-lifts (make-module-lift-context #t)]
                                               [lifts-to-module
                                                (make-lift-to-module-context
                                                 (make-parse-lifted-require m-ns self requires+provides
                                                                            #:declared-submodule-names declared-submodule-names)
                                                 #:shared-module-ends module-ends
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
                                   #:declared-submodule-names declared-submodule-names
                                   #:loop phase-1-and-2-loop))

         ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         ;; Pass 2: finish expanding expressions
         
         (define body-ctx (struct-copy expand-context partial-body-ctx
                                       [only-immediate? #f]
                                       [frame-id #:parent root-expand-context #f]
                                       [post-expansion-scope #:parent root-expand-context #f]
                                       [lifts-to-module
                                        (make-lift-to-module-context
                                         (make-parse-lifted-require m-ns self requires+provides
                                                                    #:declared-submodule-names declared-submodule-names)
                                         #:shared-module-ends module-ends
                                         #:end-as-expressions? #t)]))
         
         (finish-expanding-body-expressons partially-expanded-bodys
                                           #:tail? (zero? phase)
                                           #:phase phase
                                           #:ctx body-ctx
                                           #:self self
                                           #:declared-submodule-names declared-submodule-names)))

     ;; Check that any tentatively allowed reference at phase >= 1 is ok
     (check-defined-by-now need-eventually-defined self)
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Pass 3: resolve provides at all phases
     
     (define fully-expanded-bodys-except-post-submodules
       (resolve-provides expression-expanded-bodys
                         #:original s
                         #:requires-and-provides requires+provides
                         #:declared-submodule-names declared-submodule-names
                         #:namespace m-ns
                         #:phase phase
                         #:self self
                         #:ctx ctx))

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
                                     [frame-id #:parent root-expand-context #f]
                                     [post-expansion-scope #:parent root-expand-context #f]))
     
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
                               #:requires-and-provides requires+provides
                               #:enclosing-is-cross-phase-persistent? is-cross-phase-persistent?
                               #:declared-submodule-names declared-submodule-names
                               #:ctx ctx))
     
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
                          [module-begin-k module-begin-k])))

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;; Assemble the `module` result
   
   ;; Shift the "self" reference that we have been using for expansion
   ;; to a generic and constant (for a particualr submodule path)
   ;; "self", so that we can reocognize it for compilation or to shift
   ;; back on any future re-expansion:
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
                             #:initial-require-s initial-require-s
                             #:m-ns m-ns
                             #:ctx ctx 
                             #:phase phase
                             #:s s)
  (define (make-mb-ctx)
    (struct-copy expand-context ctx
                 [context 'module-begin]
                 [only-immediate? #t]))
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
        (expand (car bodys) (make-mb-ctx)))
      (cond
       [(eq? '#%module-begin (core-form-sym partly-expanded-body phase))
        ;; Yes, it expanded to `#%module-begin`
        partly-expanded-body]
       [else
        ;; No, it didn't expand to `#%module-begin`
        (add-module-begin (list partly-expanded-body) s initial-require-s phase (make-mb-ctx))])])]
   [else
    ;; Multiple body forms definitely need a `#%module-begin` wrapper
    (add-module-begin bodys s initial-require-s phase (make-mb-ctx))]))

;; Add `#%module-begin`, because it's needed
(define (add-module-begin bodys s initial-require-s phase mb-ctx)
  (define mb-id (datum->syntax initial-require-s '#%module-begin))
  ;; If `mb-id` is not bound, we'd like to give a clear error message
  (unless (resolve mb-id phase)
    (error "no #%module-begin binding in the module's language" s))
  (define mb (rebuild
              s
              `(,mb-id ,@bodys)))
  (define partly-expanded-mb (expand mb mb-ctx))
  (unless (eq? '#%module-begin (core-form-sym partly-expanded-mb phase))
    (error "expansion of #%module-begin is not a #%plain-module-begin form" partly-expanded-mb))
  partly-expanded-mb)
  
;; ----------------------------------------

;; Make function to adjust syntax that appears in the original module body
(define (make-apply-module-scopes inside-scope outside-scope
                                  init-ctx keep-enclosing-scope-at-phase
                                  self enclosing-self enclosing-mod)
  (lambda (s)
    (define s-without-enclosing
      (if keep-enclosing-scope-at-phase
          ;; Keep enclosing module scopes for `(module* _ #f ....)`
          s
          ;; Remove the scopes of the top level or a module outside of
          ;; this module, as well as any relevant use-site scopes
          (remove-use-site-scopes
           (for/fold ([s s]) ([sc (in-list (root-expand-context-module-scopes init-ctx))])
             (remove-scope s sc))
           init-ctx)))
    ;; Add outside- and inside-edge scopes
    (define s-with-edges
      (add-scope (add-scope s-without-enclosing
                            outside-scope)
                 inside-scope))
    (define s-with-suitable-enclosing
      (cond
       [keep-enclosing-scope-at-phase
        ;; Shift any references to the enclosing module to be relative to the
        ;; submodule
        (syntax-module-path-index-shift
         s-with-edges
         enclosing-self
         enclosing-mod)]
       [else s-with-edges]))
    ;; In case we're expanding syntax that was previously expanded,
    ;; shift the generic "self" to the "self" for the current expansion:
    (syntax-module-path-index-shift
     s-with-suitable-enclosing
     (make-generic-self-module-path-index self)
     self)))
  
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
                                #:declared-submodule-names declared-submodule-names
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
       ;; Save any requires and provides lifted during partial expansion
       (get-and-clear-requires-and-provides! (expand-context-lifts-to-module partial-body-ctx))
       ;; Ditto for expressions
       (get-and-clear-lifts! (expand-context-lifts partial-body-ctx))
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
          (define ct-m-ns (namespace->namespace-at-phase m-ns (add1 phase)))
          (eval-nested-bodys nested-bodys (add1 phase) ct-m-ns self)
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
                                       requires+provides
                                       #:declared-submodule-names declared-submodule-names)
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
            (expand-submodule ready-body self partial-body-ctx
                              #:declared-submodule-names declared-submodule-names))
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
                              scoped-ids
                              rhs))
                       inside-scope))))

(define (add-post-expansion-scope bodys ctx)
  (define sc (root-expand-context-post-expansion-scope ctx))
  (for/list ([body (in-list bodys)])
    (add-scope body sc)))

;; ----------------------------------------

;; Pass 2 of `module` expansion, which expands all expressions
(define (finish-expanding-body-expressons partially-expanded-bodys
                                          #:tail? tail?
                                          #:phase phase
                                          #:ctx body-ctx
                                          #:self self
                                          #:declared-submodule-names declared-submodule-names)
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
      (define exp-body
        (case (core-form-sym (car bodys) phase)
          [(define-values)
           (define m (match-syntax (car bodys) '(define-values (id ...) rhs)))
           (define exp-rhs (expand (m 'rhs) (as-named-context (as-expression-context body-ctx)
                                                              (m 'id))))
           (rebuild (car bodys)
                    `(,(m 'define-values) ,(m 'id) ,exp-rhs))]
          [(define-syntaxes #%require #%provide begin-for-syntax module module* #%declare)
           (car bodys)]
          [else
           (expand (car bodys) (as-expression-context body-ctx))]))
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
                                       body-ctx
                                       #:declared-submodule-names declared-submodule-names))
      (append
       lifted-requires-and-provides
       lifts
       lifted-modules
       (cons exp-body
             (loop tail? (cdr bodys))))])))

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
                          #:declared-submodule-names declared-submodule-names
                          #:namespace m-ns
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
                                       phase (struct-copy expand-context ctx
                                                          [context 'top-level]
                                                          [phase phase]
                                                          [namespace (namespace->namespace-at-phase m-ns phase)]
                                                          [requires+provides requires+provides]
                                                          [declared-submodule-names declared-submodule-names])
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
    (declare-module-from-linklet-directory!
     (compile-module tmp-mod
                     (make-compile-context #:namespace m-ns
                                           #:self enclosing-self
                                           #:root-module-name root-module-name)
                     #:self self
                     #:as-submodule? #t)
     #:as-submodule? #t)))

;; ----------------------------------------

;; Pass 4 of `module` expansion, which expands `module*` forms;
;; this pass muct happen after everything else for the module, since a
;; `module*` submodule can require from its enclosing module
(define (expand-post-submodules fully-expanded-bodys-except-post-submodules
                                #:declare-enclosing declare-enclosing-module
                                #:original s
                                #:phase phase
                                #:self self
                                #:requires-and-provides requires+provides
                                #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                                #:declared-submodule-names declared-submodule-names
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
                                 #:enclosing-requires+provides requires+provides
                                 #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                                 #:declared-submodule-names declared-submodule-names))
             (syntax-shift-phase-level submod phase)]
            [else
             (expand-submodule (car bodys) self submod-ctx
                               #:declared-submodule-names declared-submodule-names)]))
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
            (define s (string->unreadable-symbol (format "~a.~a" sym pos)))
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
       (compiled-top-run
        (compile-top body (make-compile-context
                           #:namespace m-ns
                           #:phase phase
                           #:compile-time-for-self self)
                     #:serializable? #f)
        m-ns)])))

;; ----------------------------------------

(define (expand-submodule s self ctx
                          #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                          #:enclosing-requires+provides [enclosing-r+p #f]
                          #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f]
                          #:declared-submodule-names declared-submodule-names)
  ;; Register name and check for duplicates
  (define m (match-syntax s '(module name . _)))
  (define name (syntax-e (m 'name)))
  (when (hash-ref declared-submodule-names name #f)
    (error "submodule already declared with the same name:" name m))
  (hash-set! declared-submodule-names name (syntax-e (m 'module)))

  (define submod
    (expand-module s
                   (struct-copy expand-context ctx
                                [context 'module]
                                [only-immediate? #f]
                                [post-expansion-scope #:parent root-expand-context #f])
                   self
                   #:keep-enclosing-scope-at-phase keep-enclosing-scope-at-phase
                   #:enclosing-requires+provides enclosing-r+p
                   #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?))
  
  ;; Compile and declare the submodule for use by later forms
  ;; in the enclosing module:
  (define ns (expand-context-namespace ctx))
  (define root-module-name (resolved-module-path-root-name
                            (module-path-index-resolve self)))
  (parameterize ([current-namespace ns]
                 [current-module-declare-name (make-resolved-module-path root-module-name)])
    (declare-module-from-linklet-directory!
     (compile-module submod 
                     (make-compile-context #:namespace ns
                                           #:self self
                                           #:root-module-name root-module-name)
                     #:as-submodule? #t)
     #:as-submodule? #t))

  ;; Return the expanded submodule
  submod)

;; Expand `module` forms, leave `module*` forms alone:
(define (expand-non-module*-submodules bodys phase self ctx
                                       #:declared-submodule-names declared-submodule-names)
  (for/list ([body (in-list bodys)])
    (case (core-form-sym body phase)
      [(module)
       (expand-submodule body self ctx
                         #:declared-submodule-names declared-submodule-names)]
      [else body])))

;; ----------------------------------------

(define (make-parse-lifted-require m-ns self requires+provides
                                   #:declared-submodule-names declared-submodule-names)
  (lambda (s phase)
    (define m (match-syntax s '(#%require req)))
    (parse-and-perform-requires! (list (m 'req)) self
                                 m-ns phase
                                 requires+provides
                                 #:declared-submodule-names declared-submodule-names)))
