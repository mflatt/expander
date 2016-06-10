#lang racket/base
(require (prefix-in new: "../syntax/syntax.rkt")
         (prefix-in new: "../syntax/scope.rkt")
         "syntax-to-host-syntax.rkt"
         "host-syntax-to-syntax.rkt")

(provide synthesize-reader-bridge-module)

(define orig-eval (current-eval))
(define orig-compile (current-compile))
(define orig-resolver (current-module-name-resolver))

;; Given a reader for the hosted module system and syntax,
;; declare a module in the host module system that provides
;; a reader for the host's syntax system
(define (synthesize-reader-bridge-module mod-path reader)
  (define name (module-path-index-resolve (module-path-index-join mod-path #f)))
  (unless (module-declared? name)
    (define (root-of n) (if (pair? n) (car n) n))
    (parameterize ([current-module-declare-name (make-resolved-module-path
                                                 (root-of (resolved-module-path-name name)))]
                   [current-eval orig-eval]
                   [current-compile orig-compile]
                   [current-module-name-resolver orig-resolver])
      (eval `(,(namespace-module-identifier) mod '#%kernel
              (#%provide read-syntax)
              (define-values (read-syntax)
                ,(if (procedure-arity-includes? reader 6)
                     (lambda (name in modname line col pos)
                       (syntax->host-syntax (reader name in (host-syntax->syntax modname) line col pos)))
                     (lambda (name in)
                       (syntax->host-syntax (reader name in)))))
              (module* reader #f
                (#%provide read-syntax)))))))
