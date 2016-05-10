#lang racket/base
(require "main.rkt")

;; ----------------------------------------

(define demo-ns (make-empty-core-namespace))

(namespace-require ''#%core demo-ns)
(namespace-require '(for-syntax '#%core) demo-ns)

(define (expand-expression e)
  (expand (namespace-syntax-introduce (datum->syntax #f e) demo-ns)
          demo-ns))

(define (compile+eval-expression e)
  (define c
    (compile (expand-expression e) demo-ns))
  (values c
          (eval c)))

(define (eval-expression e #:check [check-val #f])
  (define-values (c v) (compile+eval-expression e))
  (when check-val
    (unless (equal? v check-val)
      (error "check failed:" v "vs." check-val)))
  v)

(define-syntax-rule (check-print expr out ...)
  (check-thunk-print (lambda () expr) out ...))

(define (check-thunk-print t . outs)
  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    (t))
  (write-bytes (get-output-bytes o))
  (define o-expected (open-output-bytes))
  (for ([out (in-list outs)]) (println out o-expected))
  (unless (equal? (get-output-bytes o)
                  (get-output-bytes o-expected))
    (error "output check failed")))

(define-syntax-rule (check-error expr rx)
  (check-thunk-error (lambda () expr) rx))

(define (check-thunk-error t rx)
  (void)
  (with-handlers ([exn:fail? (lambda (exn)
                               (unless (regexp-match? rx (exn-message exn))
                                 (error "wrong error"))
                               `(ok ,(exn-message exn)))])
    (t)
    (error "shouldn't get here")))

;; ----------------------------------------

(compile+eval-expression
 '(case-lambda
   [(x) (set! x 5)]
   [(x y) (begin0 y x)]
   [() (with-continuation-mark 1 2 3)]))

(compile+eval-expression
 '(lambda (x) (define-values (y) x) y))

(compile+eval-expression
 '(lambda (x)
   (define-syntaxes (y) (lambda (stx) (quote-syntax 7)))
   y))

(compile+eval-expression
 '(let-values ([(z) 9])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])
    ([(x) 5] [(y) (lambda (z) z)])
    (let-values ([(z) 10])
      (begin z (if (m 10) 1 2))))))

"expansion not captured"
(eval-expression
 #:check 'x-1
 '(let-values ([(x) 'x-1])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (quote-syntax x))])
    ()
    (let-values ([(x) 'x-3])
      (m)))))

"non-capturing expansion"
(eval-expression
 #:check 'x-3
 '(let-values ([(x) 'x-1])
   (letrec-syntaxes+values
    ([(m) (lambda (stx)
            (datum->syntax
             #f
             (list (quote-syntax let-values)
                   (list (list (list (quote-syntax x))
                               (quote-syntax 'x-2)))
                   (car (cdr (syntax-e stx))))))])
    ()
    (let-values ([(x) 'x-3])
      (m x)))))

"distinct generated variables"
(eval-expression
 #:check '(2 1)
 '(letrec-syntaxes+values
   ([(gen) (lambda (stx)
             (let-values ([(vals) (syntax-e (car (cdr (syntax-e stx))))]
                          [(binds) (syntax-e (car (cdr (cdr (syntax-e stx)))))]
                          [(refs) (syntax-e (car (cdr (cdr (cdr (syntax-e stx))))))])
               (datum->syntax
                #f
                (if (null? vals)
                    (list (quote-syntax bind) binds refs)
                    (list (quote-syntax gen)
                          (cdr vals)
                          (cons (list (list (quote-syntax x))
                                      (car vals))
                                binds)
                          (cons (quote-syntax x)
                                refs))))))]
    [(bind) (lambda (stx)
              (let-values ([(binds) (car (cdr (syntax-e stx)))]
                           [(refs) (car (cdr (cdr (syntax-e stx))))])
                (datum->syntax
                 (quote-syntax here)
                 (list (quote-syntax let-values)
                       binds
                       (cons (quote-syntax list)
                             refs)))))])
   ()
   (gen (1 2) () ())))

"use-site scopes (so not ambiguous)"
(eval-expression
 #:check 'ok
 '((let-values ()
     (define-syntaxes (identity)
       (lambda (stx)
         (let-values ([(misc-id) (car (cdr (syntax-e stx)))])
           (datum->syntax
            (quote-syntax here)
            (list 'lambda '(x)
                  (list 'let-values (list
                                     (list (list misc-id) ''other))
                        'x))))))
     (identity x))
   'ok))

"use-site scope remove from binding position"
(eval-expression
 #:check 'still-ok
 '(let-values ()
   (define-syntaxes (define-identity)
     (lambda (stx)
       (let-values ([(id) (car (cdr (syntax-e stx)))])
         (datum->syntax
          (quote-syntax here)
          (list 'define-values (list id) '(lambda (x) x))))))
   (define-identity f)
   (f 'still-ok)))

"compile-time scopes pruned by `quote-syntax`"
(namespace-require '(for-meta 2 '#%core) demo-ns)
(eval-expression
 #:check 'bound
 '(letrec-syntaxes+values
   ([(m)
     (lambda (stx)
       (let-values ([(id1) (let-values ([(x) 1])
                             (define-syntaxes (wrap) ; to provoke a use-site scope
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (wrap (quote-syntax x)))]
                    [(id2) (let-values ([(x) 1])
                             (define-syntaxes (wrap)
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (wrap (quote-syntax x)))])
         (datum->syntax
          (quote-syntax here)
          (list 'let-values (list (list (list id1) ''bound))
                id2))))])
   ()
   (m)))

"`(quote-syntax .... #:local)` doesn't prune"
(eval-expression
 #:check 'bound-2
 '(letrec-syntaxes+values
   ([(m)
     (lambda (stx)
       (let-values ([(id1) (let-values ([(x) 1])
                             (quote-syntax x #:local))]
                    [(id2) (let-values ([(x) 1])
                             (define-syntaxes (wrap)
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (quote-syntax x #:local))])
         (datum->syntax
          (quote-syntax here)
          (list 'let-values (list (list (list id1) ''bound-1)
                                  (list (list id2) ''bound-2))
                id2))))])
   ()
   (m)))

"non-transformer binding misuse"
(check-error
 (expand-expression '(letrec-syntaxes+values
                      ([(v) 1])
                      ()
                      v))
 #rx"illegal use of syntax")

"free-identifier=? and bound-identifier=?"
(eval-expression
 #:check '(a (#t #f #t)
           b (#f #f #t)
           c (#t #f #t)
           d (#t #f #f)
           e (#f #f #t) (#f #f #f)
           f ((#t #f) (#f #f)))
 '(let-values ([(x) 0])
   (letrec-syntaxes+values
    ([(m) (lambda (stx)
            (datum->syntax
             (quote-syntax here)
             (list (quote-syntax quote)
                   (list
                    (free-identifier=? (quote-syntax x) (car (cdr (syntax-e stx))))
                    (bound-identifier=? (quote-syntax x) (car (cdr (syntax-e stx))))
                    (bound-identifier=? (car (cdr (syntax-e stx)))
                                        (car (cdr (cdr (syntax-e stx)))))))))])
    ()
    (list 
     'a
     (m x x)
     'b
     (let-values ([(x) 1])
       (m x x))
     'c
     (letrec-syntaxes+values
      ([(n) (lambda (stx)
              (quote-syntax (m x x)))])
      ()
      (n))
     'd
     (letrec-syntaxes+values
      ([(o) (lambda (stx)
              (datum->syntax
               (quote-syntax here)
               (list (quote-syntax m)
                     (car (cdr (syntax-e stx)))
                     (quote-syntax x))))])
      ()
      (o x))
     'e
     (m not-x not-x)
     (m not-x also-not-x)
     'f
     (letrec-syntaxes+values
      ([(p) (lambda (stx)
              (letrec-syntaxes+values
               ([(q) (lambda (nested-stx)
                       (datum->syntax
                        (quote-syntax here)
                        (list (quote-syntax quote)
                              ;; These `free-identifier=?` test should be at phase 1:
                              (list
                               (free-identifier=? (quote-syntax stx) (car (cdr (syntax-e nested-stx))))
                               (free-identifier=? (quote-syntax stx) (car (cdr (cdr (syntax-e nested-stx)))))))))])
               ()
               (datum->syntax
                (quote-syntax here)
                (list (quote-syntax quote)
                      (list (q stx not-stx)
                            (let-values ([(stx) 0])
                              (q stx stx)))))))])
      ()
      (p))))))

;; ----------------------------------------

(define (eval-module-declaration mod)
  (parameterize ([current-namespace demo-ns])
    (eval-expression mod)))

(eval-module-declaration '(module m1 '#%core
                           (#%require (for-syntax '#%core))
                           (begin-for-syntax
                             (define-values (ten) (quote-syntax 10)))
                           (define-syntaxes (m) (lambda (stx) ten))
                           (define-values (x) 1)
                           (println x)
                           (#%provide (prefix-all-defined def:))
                           (println (m))
                           (m)))

(eval-module-declaration '(module m2 '#%core
                           (#%require 'm1)
                           (println def:x)))

(check-print
 (namespace-require ''m2 demo-ns)
 1
 10
 1)

(eval-module-declaration '(module with-use-site-scope '#%core
                           (#%require (for-syntax '#%core))

                           (define-syntaxes (identity)
                             (lambda (stx)
                               (let-values ([(misc-id) (car (cdr (syntax-e stx)))])
                                 (datum->syntax
                                  (quote-syntax here)
                                  (list 'lambda '(x)
                                        (list 'let-values (list
                                                           (list (list misc-id) ''other))
                                              'x))))))
                           (identity x)

                           (define-syntaxes (define-identity)
                             (lambda (stx)
                               (datum->syntax
                                #f
                                (list (quote-syntax define-values)
                                      (list (car (cdr (syntax-e stx))))
                                      (quote-syntax  (lambda (x) x))))))
                           (define-identity f)
                           (println (f 5))))

(check-print
 (namespace-require ''with-use-site-scope demo-ns)
 5)

(eval-module-declaration '(module definition-shadows-initial-require '#%core
                           (#%require (rename '#%core orig:list list))
                           (#%provide list)
                           (define-values (list)
                             (lambda (a b)
                               (println a)
                               (orig:list a b)))))

(eval-module-declaration '(module require-shadows-initial-require '#%core
                           (#%require 'definition-shadows-initial-require)
                           (list 'a 'b)))

(check-print
 (namespace-require ''require-shadows-initial-require demo-ns)
 'a)

(check-error
 (eval-module-declaration '(module m '#%core
                            (#%require '#%core)
                            (#%provide list)
                            (define-values (list) 0)))
 #rx"already required or defined")

;; ----------------------------------------

(eval-module-declaration '(module random-n '#%core
                           (define-values (n) (random))
                           (#%provide n)))

(eval-module-declaration '(module use-random-n '#%core
                           (#%require 'random-n
                                      (for-syntax '#%core
                                                  'random-n))
                           (define-syntaxes (m)
                             (lambda (stx) (datum->syntax (quote-syntax here)
                                                     n)))
                           (println (m))
                           (println (m))
                           (println n)
                           (println n)))

"print same number twive, then different number twice"
(namespace-require ''use-random-n demo-ns)

;; ----------------------------------------

;; Fresh compile-time, same run-time:
(eval-module-declaration '(module use-random-n-again '#%core
                           (#%require 'random-n
                                      (for-syntax '#%core
                                                  'random-n))
                           (define-syntaxes (m)
                             (lambda (stx) (datum->syntax (quote-syntax here)
                                                     n)))
                           (println (m))
                           (println n)))

"first number is fresh, second number is same"
(namespace-require ''use-random-n-again demo-ns)

;; ----------------------------------------

;; Check phase shifting of syntax objects:
(eval-module-declaration '(module two-xes '#%core
                           (#%require (for-syntax '#%core))
                           (define-values (x) 0)
                           (begin-for-syntax
                            (define-values (x) 1))
                           (#%provide x
                                      (for-syntax x))))

(eval-module-declaration '(module use-two-xes '#%core
                           (#%require (for-template 'two-xes)
                                      (for-syntax '#%core))
                           (define-values (rt-x-ref) (quote-syntax x))
                           (begin-for-syntax
                             (define-values (ct-x-ref) (quote-syntax x)))
                           (#%provide rt-x-ref
                                      (for-syntax ct-x-ref))))

(eval-module-declaration '(module use-x-ref '#%core
                           (#%require 'use-two-xes
                                      (for-syntax '#%core
                                                  'use-two-xes))
                           (define-syntaxes (ct-m) (lambda (stx) ct-x-ref))
                           (define-syntaxes (rt-m) (lambda (stx) rt-x-ref))
                           (println (ct-m))
                           (println (rt-m))))

(check-print
 (namespace-require ''use-x-ref demo-ns)
 1
 0)

;; ----------------------------------------

;; Custom `#%module-begin'
(eval-module-declaration '(module printing-mb '#%core
                           (#%require (for-syntax '#%core))
                           (#%provide (all-from-except '#%core #%module-begin)
                                      (rename module-begin #%module-begin))
                           (define-syntaxes (module-begin)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (cons
                                 (quote-syntax #%module-begin)
                                 (map (lambda (b)
                                        (datum->syntax
                                         (quote-syntax here)
                                         (list (quote-syntax println) b)))
                                      (cdr (syntax-e stx)))))))))

(eval-module-declaration '(module printed 'printing-mb
                           (+ 1 2)
                           (+ 3 4)))

(check-print
 (namespace-require ''printed demo-ns)
 3
 7)

;; ----------------------------------------

;; Submodule

(eval-module-declaration '(module with-pre-submodule '#%core
                           (module a '#%core
                             (#%provide a)
                             (define-values (a) 'a))
                           (#%require (submod "." a))
                           (println a)))

(check-print
 (namespace-require ''with-pre-submodule demo-ns)
 'a)

(eval-module-declaration '(module with-post-submodule '#%core
                           (#%provide b)
                           (define-values (b) 'b)
                           (module* b '#%core
                             (#%require (submod ".."))
                             (println b))))

(check-print
 (namespace-require '(submod 'with-post-submodule b) demo-ns)
 'b)

(eval-module-declaration '(module with-#f-submodule '#%core
                           (define-values (c) 'c)
                           (module* c #f
                             (println c))))

(check-print
 (namespace-require '(submod 'with-#f-submodule c) demo-ns)
 'c)

(eval-module-declaration '(module with-shifted-#f-submodule '#%core
                           (#%require (for-syntax '#%core))
                           (define-values (d) 'd)
                           (begin-for-syntax
                             (define-values (d-stx) (quote-syntax d))
                             (module* d #f
                               (#%provide get-d-stx)
                               (define-values (get-d-stx) (lambda () d-stx))))))

(eval-module-declaration '(module use-shifted-#f-submodule '#%core
                           (#%require (for-syntax '#%core
                                                  (submod 'with-shifted-#f-submodule d)))
                           (define-syntaxes (m) (lambda (stx) (get-d-stx)))
                           (println (m))))

(check-print
 (namespace-require ''use-shifted-#f-submodule demo-ns)
 'd)

(eval-module-declaration '(module with-#f-submodule-provide '#%core
                           (define-values (e) 'e)
                           (module* e #f
                             (#%provide e))))

(eval-module-declaration '(module use-submodule-provide '#%core
                           (#%require (submod 'with-#f-submodule-provide e))
                           (println e)))

(check-print
 (namespace-require ''use-submodule-provide demo-ns)
 'e)

(eval-module-declaration '(module expand-provide '#%core
                           (#%require (for-syntax '#%core))
                           (define-values (x) 'x)
                           (define-syntaxes (m) (lambda (stx) (quote-syntax (begin x))))
                           (#%provide (expand (m)))))

(eval-module-declaration '(module use-expand-provide '#%core
                           (#%require 'expand-provide)
                           (println x)))

(check-print
 (namespace-require ''use-expand-provide demo-ns)
 'x)

