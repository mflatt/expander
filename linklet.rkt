#lang racket/base
(require "set.rkt"
         "datum-map.rkt"
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
         compiled-linklet-variables
         eval-linklet                ; serializable to instantiable
         instantiate-linklet         ; produces an instance given instances

         make-instance
         instance-variable-value
         set-instance-variable-value!

         linklet-directory?       ; maps byte strings to linking units and nested cds
         hash->linklet-directory  ; converts a hash table to a cd
         linklet-directory->hash  ; the other way
         encode-linklet-directory-key ; S-expresion -> suitable byte string for a cd key

         linklet-compile-to-s-expr) ; a parameter; whether to "compile" to a source form

(struct linklet (code)
        #:prefab)
(struct instance (variables))

(define (make-instance)
  (instance (make-hasheq)))

(define (instance-variable-box i sym can-create?)
  (or (hash-ref (instance-variables i) sym #f)
      (if can-create?
          (let ([b (box unsafe-undefined)])
            (hash-set! (instance-variables i) sym b)
            b)
          (error 'link "missing binding: ~s" sym))))

(define (set-instance-variable-value! i sym val)
  (set-box! (instance-variable-box i sym #t) val))

(define (instance-variable-value i sym [fail-k (lambda () (error "instance variable not found:" sym))])
  (define b (hash-ref (instance-variables i) sym #f))
  (cond
   [b (unbox b)]
   [(procedure? fail-k) (fail-k)]
   [else fail-k]))

;; ----------------------------------------

(define cu-namespace (make-base-empty-namespace))
(parameterize ([current-namespace cu-namespace])
  (namespace-require ''#%kernel)
  (namespace-require 'racket/unsafe/undefined)
  (namespace-set-variable-value! 'instance-variable-box instance-variable-box))

;; ----------------------------------------

(define (desugar-linklet c)
  (unless (eq? '#:import (list-ref c 1)) (error "bad linklet syntax" c))
  (define imports (list-ref c 2))
  (unless (eq? '#:export (list-ref c 3)) (error "bad linklet syntax" c))
  (define exports (list-ref c 4))
  (define bodys (list-tail c 5))
  (define box-bindings
    `(,@(for*/list ([inst-imports (in-list imports)]
                    [inst (in-value (car inst-imports))]
                    [name (in-list (cdr inst-imports))])
          (define ext (if (symbol? name) name (car name)))
          (define int (if (symbol? name) name (cadr name)))
          `[(,int) (instance-variable-box ,inst ',ext #f)])
      ,@(for/list ([name (in-list exports)])
          (define int (if (symbol? name) name (car name)))
          (define ext (if (symbol? name) name (cadr name)))
          `[(,int) (instance-variable-box self-inst ',ext #t)])))
  (define box-syms (apply seteq (map caar box-bindings)))
  (define (desugar e)
    (cond
     [(symbol? e) (if (set-member? box-syms e)
                      `(unbox ,e)
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
        [else (map desugar e)])]
     [else e]))
  `(lambda (self-inst ,@(map car imports))
    (let-values ,box-bindings
      (begin
        ,@(for/list ([body (in-list bodys)])
            (desugar body))
        (void)))))

;; Only `#:export`-listed names count as "variables"
(define (extract-variables-from-expression c)
  ;; position 4 is after `#:exports`
  (for/list ([e (in-list (list-ref c 4))])
    (if (symbol? e)
        e
        (cadr e))))

;; ----------------------------------------

(define orig-eval (current-eval))

(define linklet-compile-to-s-expr (make-parameter #f))

;; Compile to a serializable form
(define (compile-linklet c)
  (cond
   [(linklet-compile-to-s-expr)
    (de-path c)]
   [else
    (define plain-c (desugar-linklet c))
    (parameterize ([current-namespace cu-namespace]
                   [current-eval orig-eval])
      ;; Use a vector to list the exported variables
      ;; with the compiled bytecode
      (vector (compile plain-c)
              (extract-variables-from-expression c)))]))

;; Extract variable list from a compiled linklet:
(define (compiled-linklet-variables lu)
  (if (vector? lu)
      (vector-ref lu 1)
      ;; Assumed previous "compiled" to source
      (extract-variables-from-expression lu)))

;; Convert serializable form to instantitable form
(define (eval-linklet lu)
  (parameterize ([current-namespace cu-namespace]
                 [current-eval orig-eval])
    (eval (if (vector? lu)
              ;; Normal mode: compiled to bytecode
              (vector-ref lu 0)
              ;; Assume previously "compiled" to source:
              (desugar-linklet (re-path lu))))))

;; Instantiate
(define (instantiate-linklet elu imports [i (make-instance)])
  (apply elu i imports)
  i)

;; ----------------------------------------

(struct linklet-directory (table)
        #:prefab)

(define (hash->linklet-directory ht)
  (linklet-directory ht))

(define (linklet-directory->hash cd)
  (linklet-directory-table cd))

(define (encode-linklet-directory-key v)
  (string->bytes/utf-8 (format "~a" v)))

;; ----------------------------------------

(struct path-bytes (bstr) #:prefab)
(struct void-value () #:prefab)

(define (de-path p)
  (datum-map p (lambda (tail? p)
                 (cond
                  [(path? p) (path-bytes (path->bytes p))]
                  [(void? p) (void-value)]
                  [else p]))))

(define (re-path p)
  (datum-map p
             (lambda (tail? p)
               (cond
                [(path-bytes? p) (bytes->path (path-bytes-bstr p))]
                [(void-value? p) (void)]
                [else p]))))
