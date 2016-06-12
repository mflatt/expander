#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../common/contract.rkt"
         "module.rkt")

(provide compiled-expression?

         compiled-module-expression?
         module-compiled-name
         module-compiled-language-info)

(define (compiled-expression? c)
  (or (compiled-in-memory? c)
      (linklet-directory? c)))

(define (compiled-module-expression? c)
  (define ld (if (compiled-in-memory? c)
                 (compiled-in-memory-linklet-directory c)
                 c))
  (and (linklet-directory? ld)
       (hash-ref (linklet-directory->hash ld) #".decl" #f)))

(define module-compiled-name
  (case-lambda
    [(c)
     (check 'module-compiled-name compiled-module-expression? c)
     (define inst (compiled-module->declaration-instance c))
     (instance-variable-value inst 'default-name)]
    [(c name)
     (check 'module-compiled-name compiled-module-expression? c)
     (unless (or (symbol? name)
                 (and (pair? name)
                      (list? name)
                      (andmap symbol? name)))
       (raise-argument-error 'module-compiled-name
                             "(or/c symbol? (cons/c symbol? (non-empty-listof symbol?)))"
                             name))
     (error "not yet implemented")]))

(define (module-compiled-language-info c)
  (check 'module-compiled-language-info compiled-module-expression? c)  
  (define inst (compiled-module->declaration-instance c))
  (instance-variable-value inst 'language-info))
