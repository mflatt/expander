#lang racket/base
(require "set.rkt"
         "datum-map.rkt"
         "built-in-symbol.rkt"
         racket/unsafe/undefined)

;; A "linklet" is intended as the primitive form of separate (not
;; necessarily independent) compilation and linking. A `linklet` form
;; compiles to a serializable linklet, a serializable linklet can be
;; converted to an instantiable linklet, and instantiation of a
;; linklet produces an "instance" given other instances to satisfy its
;; imports. An instance, which essentially just maps symbols to
;; values, can also be created directly, so it serves as the bridge
;; between the worlds of values and compiled objects.

;; Since Racket doesn't yet support linklets natively, we implement
;; them here by compilaing `linklet` to `lambda`.

;; A "linklet directory" is similarly intended as a primitive
;; constructs that is essentially a mapping of byte strings to
;; linklets and sub-directories. The intent is that individual
;; linklets can be efficiently extracted from the marshaled form of a
;; linklet directory --- the primitive form of accessing an indvidual
;; submodule.

(provide compile-linklet             ; result is serializable
         eval-linklet                ; serializable to instantiable
         instantiate-linklet         ; fills in an instance given argument instances
         
         compiled-linklet-import-variables
         compiled-linklet-export-variables

         make-instance
         instance-name               ; a "name" can be any data
         instance-variable-names
         instance-variable-value
         instance-set-variable-value!
         instance-unset-variable!

         lookup-primitive-instance

         linklet-directory?       ; maps byte strings to linking units and nested cds
         hash->linklet-directory  ; converts a hash table to a cd
         linklet-directory->hash  ; the other way
         encode-linklet-directory-key ; S-expresion -> suitable byte string for a cd key
         
         variable-reference?
         variable-reference->instance
         variable-reference-constant?
         
         linklet-compile-to-s-expr) ; a parameter; whether to "compile" to a source form

(struct instance (name        ; any value (e.g., a namespace)
                  variables)) ; symbol -> value

(define (make-instance name)
  (instance name (make-hasheq)))

(define (instance-variable-names i)
  (hash-keys (instance-variables i)))

(define (instance-variable-box i sym can-create?)
  (or (hash-ref (instance-variables i) sym #f)
      (if can-create?
          (let ([b (box undefined)])
            (hash-set! (instance-variables i) sym b)
            b)
          (error 'link "missing binding: ~s" sym))))

(define (instance-set-variable-value! i sym val)
  (set-box! (instance-variable-box i sym #t) val))

(define (instance-unset-variable! i sym)
  (set-box! (instance-variable-box i sym #t) undefined))

(define (instance-variable-value i sym [fail-k (lambda () (error "instance variable not found:" sym))])
  (define b (hash-ref (instance-variables i) sym #f))
  (cond
   [(and b
         (not (eq? (unbox b) undefined)))
    (unbox b)]
   [(procedure? fail-k) (fail-k)]
   [else fail-k]))

;; ----------------------------------------

(define undefined (gensym 'undefined))

(define (check-not-undefined val sym)
  (if (eq? val undefined)
      (check-not-unsafe-undefined unsafe-undefined sym)
      val))

;; ----------------------------------------

(define (lookup-primitive-instance name)
  (define mod-name `(quote ,name))
  (define-values (vars trans) (module->exports mod-name))
  (instance name (for/hash ([sym (in-list (map car (cdr (assv 0 vars))))])
                   (values sym
                           (box (dynamic-require mod-name sym))))))

;; ----------------------------------------

(struct variable-reference (instance primitive-varref))

(define (variable-reference->instance vr)
  (variable-reference-instance vr))

(define variable-reference-constant?*
  (let ([variable-reference-constant?
         (lambda (vr)
           (variable-reference-constant? (variable-reference-primitive-varref vr)))])
    variable-reference-constant?))

;; ----------------------------------------

(define cu-namespace (make-base-empty-namespace))
(parameterize ([current-namespace cu-namespace])
  (namespace-require ''#%kernel)
  (namespace-set-variable-value! 'check-not-undefined check-not-undefined)
  (namespace-set-variable-value! 'instance-variable-box instance-variable-box)
  (namespace-set-variable-value! 'variable-reference variable-reference)
  (namespace-set-variable-value! 'variable-reference? variable-reference? #t)
  (namespace-set-variable-value! 'variable-reference->instance variable-reference->instance #t)
  (namespace-set-variable-value! 'variable-reference-constant? variable-reference-constant?* #t))

;; ----------------------------------------

(define (desugar-linklet c)
  (unless (eq? '#:import (list-ref c 1)) (error "bad linklet syntax" c))
  (define imports (list-ref c 2))
  (unless (eq? '#:export (list-ref c 3)) (error "bad linklet syntax" c))
  (define exports (list-ref c 4))
  (define bodys (list-tail c 5))
  (define import-box-bindings
    (for*/list ([inst-imports (in-list imports)]
                [inst (in-value (car inst-imports))]
                [name (in-list (cdr inst-imports))])
      (define ext (if (symbol? name) name (car name)))
      (define int (if (symbol? name) name (cadr name)))
      `[(,int) (instance-variable-box ,inst ',ext #f)]))
  (define export-box-bindings
    (for/list ([name (in-list exports)])
      (define int (if (symbol? name) name (car name)))
      (define ext (if (symbol? name) name (cadr name)))
      `[(,int) (instance-variable-box self-inst ',ext #t)]))
  (define box-bindings (append import-box-bindings export-box-bindings))
  (define import-box-syms (apply seteq (map caar import-box-bindings)))
  (define box-syms (set-union import-box-syms
                              (apply seteq (map caar export-box-bindings))))
  (define (desugar e)
    (cond
     [(symbol? e) (if (set-member? box-syms e)
                      (if (set-member? import-box-syms e)
                          `(unbox ,e)
                          `(check-not-undefined (unbox ,e) ',e))
                      e)]
     [(pair? e)
      (case (car e)
        [(quote) e]
        [(set!) (if (set-member? box-syms (cadr e))
                    `(set-box! ,(cadr e) ,(desugar (caddr e)))
                    `(set! ,(cadr e) ,(desugar (caddr e))))]
        [(define-values)
         (define ids (cadr e))
         (define tmps (map gensym ids))
         `(define-values ,(for/list ([id (in-list ids)]
                                     #:when (not (set-member? box-syms id)))
                            id)
           (let-values ([,tmps (let-values ([,ids ,(desugar (caddr e))])
                                 (values ,@ids))])
             (begin
               ,@(for/list ([id (in-list ids)]
                            [tmp (in-list tmps)]
                            #:when (set-member? box-syms id))
                   `(set-box! ,id ,tmp))
               (values ,@(for/list ([id (in-list ids)]
                                    [tmp (in-list tmps)]
                                    #:when (not (set-member? box-syms id)))
                           tmp)))))]
        [(lambda) `(lambda ,(cadr e) ,@(map desugar (cddr e)))]
        [(case-lambda)
         `(case-lambda ,@(for/list ([clause (cdr e)])
                      `[,(car clause) ,@(map desugar (cdr clause))]))]
        [(#%variable-reference)
         (if (and (pair? (cdr e))
                  (set-member? box-syms (cadr e)))
             ;; Using a plain `#%variable-reference` (for now) means
             ;; that all imported and exported variables count as
             ;; mutable:
             '(variable-reference self-inst (#%variable-reference))
             ;; Preserve info about a local identifier:
             `(variable-reference self-inst ,e))]
        [else (map desugar e)])]
     [else e]))
  `(lambda (self-inst ,@(map car imports))
    (let-values ,box-bindings
      (begin
        ,@(for/list ([body (in-list bodys)])
            (desugar body))
        (void)))))

;; -> list of list of symbols
(define (extract-import-variables-from-expression c)
  ;; position 2 is after `#:import`
  (for/list ([is (in-list (list-ref c 2))])
    (for/list ([i (in-list (cdr is))])
      (if (symbol? i)
          i
          (car i)))))

;; -> list of symbols
(define (extract-export-variables-from-expression c)
  ;; position 4 is after `#:export`
  (for/list ([e (in-list (list-ref c 4))])
    (if (symbol? e)
        e
        (cadr e))))

;; ----------------------------------------

(define orig-eval (current-eval))
(define orig-compile (current-compile))

(define linklet-compile-to-s-expr (make-parameter #f))

;; Compile to a serializable form
(define (compile-linklet c)
  (cond
   [(linklet-compile-to-s-expr)
    (de-path c)]
   [else
    (define plain-c (desugar-linklet c))
    (parameterize ([current-namespace cu-namespace]
                   [current-eval orig-eval]
                   [current-compile orig-compile])
      ;; Use a vector to list the exported variables
      ;; with the compiled bytecode
      (vector (compile plain-c)
              (extract-import-variables-from-expression c)
              (extract-export-variables-from-expression c)))]))

;; Convert serializable form to instantitable form
(define (eval-linklet linklet)
  (parameterize ([current-namespace cu-namespace]
                 [current-eval orig-eval]
                 [current-compile orig-compile])
    (eval (if (vector? linklet)
              ;; Normal mode: compiled to bytecode
              (vector-ref linklet 0)
              ;; Assume previously "compiled" to source:
              (desugar-linklet (re-path linklet))))))

;; Instantiate
(define (instantiate-linklet linklet import-instances [target-instance (make-instance 'anonymous)])
  (apply linklet target-instance import-instances)
  target-instance)

;; ----------------------------------------

(define (compiled-linklet-import-variables linklet)
  (if (vector? linklet)
      (vector-ref linklet 1)
      ;; Assumed previous "compiled" to source
      (extract-import-variables-from-expression linklet)))

(define (compiled-linklet-export-variables linklet)
  (if (vector? linklet)
      (vector-ref linklet 2)
      ;; Assumed previous "compiled" to source
      (extract-export-variables-from-expression linklet)))

;; ----------------------------------------

(struct linklet-directory (table)
        #:prefab)

(define (hash->linklet-directory ht)
  (linklet-directory ht))

(define (linklet-directory->hash ld)
  (linklet-directory-table ld))

(define (encode-linklet-directory-key v)
  (string->bytes/utf-8 (format "~a" v)))

;; ----------------------------------------

(struct path-bytes (bstr) #:prefab)
(struct void-value () #:prefab)

(define (de-path c)
  (datum-map c (lambda (tail? c)
                 (cond
                  [(path? c) (path-bytes (path->bytes c))]
                  [(void? c) (void-value)]
                  [else c]))))

(define (re-path c)
  (datum-map c
             (lambda (tail? c)
               (cond
                [(path-bytes? c) (bytes->path (path-bytes-bstr c))]
                [(void-value? c) (void)]
                [else c]))))
