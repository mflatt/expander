#lang racket/base
(require "set.rkt"
         "datum-map.rkt"
         racket/unsafe/undefined)

;; Implement compilation units and compilation directories, to be
;; replaced with a built-in implementation

(provide compile-compilation-unit
         compiled-compilation-unit-variables
         eval-compilation-unit
         instantiate-compilation-unit
         
         make-instance
         instance-variable-value
         set-instance-variable-value!

         compilation-directory?
         hash->compilation-directory
         compilation-directory->hash
         encode-compilation-directory-key
         
         compilation-unit-compile-to-s-expr)

(struct compilation-unit (code)
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

(define (desugar-compilation-unit c)
  (unless (eq? '#:import (list-ref c 1)) (error "bad compilation-unit syntax" c))
  (define imports (list-ref c 2))
  (unless (eq? '#:export (list-ref c 3)) (error "bad compilation-unit syntax" c))
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

(define compilation-unit-compile-to-s-expr (make-parameter #f))

;; Compile to a serializable form
(define (compile-compilation-unit c)
  (cond
   [(compilation-unit-compile-to-s-expr)
    (de-path c)]
   [else
    (define plain-c (desugar-compilation-unit c))
    (parameterize ([current-namespace cu-namespace]
                   [current-eval orig-eval])
      ;; Use a vector to list the exported variables
      ;; with the compiled bytecode
      (vector (compile plain-c)
              (extract-variables-from-expression c)))]))

;; Extract variable list from a compiled compilation unit:
(define (compiled-compilation-unit-variables ccu)
  (if (vector? ccu)
      (vector-ref ccu 1)
      ;; Assumed previous "compiled" to source
      (extract-variables-from-expression ccu)))

;; Convert serializable form to instantitable form
(define (eval-compilation-unit ccu)
  (parameterize ([current-namespace cu-namespace]
                 [current-eval orig-eval])
    (eval (if (vector? ccu)
              ;; Normal mode: compiled to bytecode
              (vector-ref ccu 0)
              ;; Assume previously "compiled" to source:
              (desugar-compilation-unit (re-path ccu))))))

;; Instantiate
(define (instantiate-compilation-unit cu imports [i (make-instance)])
  (apply cu i imports)
  i)

;; ----------------------------------------

(struct compilation-directory (table)
        #:prefab)

(define (hash->compilation-directory ht)
  (compilation-directory ht))

(define (compilation-directory->hash cd)
  (compilation-directory-table cd))

(define (encode-compilation-directory-key v)
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
