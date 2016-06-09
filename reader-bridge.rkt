#lang racket/base
(require (prefix-in new: "syntax.rkt")
         (prefix-in new: "scope.rkt")
         (only-in "read-syntax.rkt"
                  base:syntax->syntax))

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
                       (syntax->base:syntax (reader name in (base:syntax->syntax modname) line col pos)))
                     (lambda (name in)
                       (syntax->base:syntax (reader name in)))))
              (module* reader #f
                (#%provide read-syntax)))))))

(define (syntax->base:syntax v)
  (new:syntax-map v
                  (lambda (tail? v) v)
                  (lambda (orig-s d)
                    (datum->syntax #f d (srcloc->vector (new:syntax-srcloc orig-s))))
                  new:syntax-e))

(define (srcloc->vector s)
  (and s
       (vector (srcloc-source s)
               (srcloc-line s)
               (srcloc-column s)
               (srcloc-position s)
               (srcloc-span s))))
