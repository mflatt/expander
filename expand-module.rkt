#lang racket/unit
(require racket/unit
         "stx.rkt"
         "scope.rkt"
         "pattern.rkt"
         "namespace.rkt"
         "binding.rkt"
         "dup-check.rkt"
         "require.rkt"
         "expand-context.rkt"
         "expand-sig.rkt")

(import expand^)
(export)

;; ----------------------------------------

(define (resolve-module-path p)
  ;; Assume 'name, for now
  (cadr p))

;; ----------------------------------------

(add-core-form!
 'module
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (error "allowed only at the top level:" s))
   
   (define m (parse-syntax s '(module id initial-import body ...)))
   
   (define initial-import (syntax->datum (m 'initial-import)))
   (unless (module-path? initial-import)
     (error "not a module path:" (m 'initial-import)))
   
   (define outside-scope (new-scope))
   (define inside-scope (new-multi-scope))
   (define inside-stx (add-scope empty-stx inside-scope))
   
   (define initial-import-name (resolve-module-path initial-import))   

   (define self 'self)
   (define m-ns (make-module-namespace (expand-context-namespace ctx)
                                       'self))

   (stx-context-require! inside-stx
                         0
                         m-ns
                         initial-import-name)
   
   (define bodys (for/list ([body (in-list (m 'body))])
                   (define scopes
                     (expand-context-current-module-scopes ctx))
                   (define body-without-enclosing
                     (for/fold ([body body]) ([sc (in-list scopes)])
                       (remove-scope body sc)))
                   (add-scope (add-scope body-without-enclosing
                                         outside-scope)
                              inside-scope)))

   (define phase 0)
      
   ;; Pass 1: partially expand to discover all bindings and install all 
   ;; defined macro transformers
   
   (define partial-body-ctx (struct-copy expand-context ctx
                                         [context 'module]
                                         [phase phase]
                                         [namespace m-ns]
                                         [only-immediate? #t]
                                         [add-scope inside-scope]
                                         [current-module-scopes
                                          (list inside-scope outside-scope)]))
   (define local-names (make-hasheq))
   
   (define partially-expanded-bodys
     (let loop ([bodys bodys] [done-bodys null])
       (cond
        [(null? bodys) (reverse done-bodys)]
        [else
         (define exp-body (expand (car bodys) partial-body-ctx))
         (case (core-form-sym exp-body phase)
           [(begin)
            (define m (parse-syntax exp-body '(begin e ...)))
            (loop (append (m 'e) (cdr bodys))
                  done-bodys)]
           [(define-values)
            (define m (parse-syntax exp-body '(define-values (id ...) rhs)))
            (define ids (m 'id))
            (check-ids-unbound ids phase)
            (define keys (select-local-names-and-bind ids local-names self phase))
            (loop (cdr bodys)
                  (cons (car bodys) done-bodys))]
           [(define-syntaxes)
            (define m (parse-syntax exp-body '(define-syntaxes (id ...) rhs)))
            (define ids (m 'id))
            (check-ids-unbound ids phase)
            (define keys (select-local-names-and-bind ids local-names self phase))
            ;; Expand and evaluate RHS:
            (define-values (exp-rhs vals)
              (expand+eval-for-syntaxes-binding (m 'rhs) ids partial-body-ctx))
            ;; Install transformers in the namespace for expansion:
            (for ([key (in-list keys)]
                  [val (in-list vals)])
              (namespace-set-transformer! m-ns phase key val))
            (loop (cdr bodys)
                  (cons (rebuild (car bodys)
                                 `(,(m 'define-syntaxes) ,ids ,exp-rhs))
                        done-bodys))]
           [(#%require)
            (define m (parse-syntax exp-body '(#%require (for-syntax mod))))
            (stx-context-require! inside-stx
                                  1
                                  m-ns
                                  (resolve-module-path
                                   (syntax->datum (m 'mod))))
            (loop (cdr bodys)
                  (cons (car bodys)
                        done-bodys))]
           [else
            (loop (cdr bodys)
                  (cons (car bodys)
                        done-bodys))])])))
      
   ;; Pass 2: finish expanding
   (define body-ctx (struct-copy expand-context partial-body-ctx
                                 [only-immediate? #f]
                                 [add-scope #f]))
   
   
   (define fully-expanded-bodys
     (let loop ([bodys bodys] [done-bodys null])
       (cond
        [(null? bodys) (reverse done-bodys)]
        [else
         (case (core-form-sym (car bodys) phase)
           [(define-values)
            (define m (parse-syntax (car bodys) '(define-values (id ...) rhs)))
            (define exp-rhs (expand (m 'rhs) body-ctx))
            (loop (cdr bodys)
                  (cons (rebuild (car bodys)
                                 `(,(m 'define-values) ,(m 'id) ,exp-rhs))
                        done-bodys))]
           [(define-syntaxes #%require)
            (loop (cdr bodys)
                  (cons (car bodys)
                        done-bodys))]
           [else
            (loop (cdr bodys)
                  (cons (expand (car bodys) body-ctx)
                        done-bodys))])])))
   
   (rebuild
    s
    `(,(m 'module) ,(m 'initial-import) ,@fully-expanded-bodys))))

;; ----------------------------------------

(define (check-ids-unbound ids phase)
  (for ([id (in-list ids)])
    (when (resolve id phase #:exactly? #t)
      (error "identifier is already defined or imported:" id))))

(define (select-local-names-and-bind ids local-names self phase)
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
    (add-binding! id (module-binding self phase local-sym) phase)
    local-sym))
