#lang racket/base
(require racket/list)

(provide resolve-module-path
         build-module-name)

;; A resolved module name is either
;;  - a symbol for a top-level module
;;  - a list of two or more symbols for a submodule

(define (resolve-module-path p enclosing)
  (unless (module-path? p)
    (error "not a module path:" p))
  (cond
   [(and (list? p)
         (= (length p) 2)
         (eq? 'quote (car p))
         (symbol? (cadr p)))
    (cadr p)]
   [(and (list? p)
         (eq? 'submod (car p))
         (equal? ".." (cadr p)))
    (for/fold ([enclosing enclosing]) ([s (in-list (cdr p))])
      (build-module-name s enclosing #:original p))]
   [(and (list? p)
         (eq? 'submod (car p))
         (equal? "." (cadr p)))
    (for/fold ([enclosing enclosing]) ([s (in-list (cddr p))])
      (build-module-name s enclosing #:original p))]
   [(and (list? p)
         (eq? 'submod (car p)))
    (let ([base (resolve-module-path (cadr p) enclosing)])
      (for/fold ([enclosing base]) ([s (in-list (cddr p))])
        (build-module-name s enclosing #:original p)))]
   [else
    (error "not a supported module path:" p)]))

;; Build a submodule name given an enclosing module name, if cany
(define (build-module-name name ; a symbol
                           enclosing-module-name ; #f => no enclosing module
                           #:original [orig-name name]) ; for error reporting
  (cond
   [(not enclosing-module-name) name]
   [(symbol? enclosing-module-name) (list enclosing-module-name name)]
   [(equal? name "..")
    (cond
     [(symbol? enclosing-module-name)
      (error "too many \"..\"s:" orig-name)]
     [(= 2 (length enclosing-module-name)) (car enclosing-module-name)]
     [else (drop-right enclosing-module-name 1)])]
   [else (append enclosing-module-name (list name))]))
