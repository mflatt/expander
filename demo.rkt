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
    (error "output check failed:"
           (get-output-bytes o)
           "vs." (get-output-bytes o-expected))))

(define-syntax-rule (check-error expr rx)
  (check-thunk-error (lambda () expr) rx))

(define (check-thunk-error t rx)
  (void)
  (with-handlers ([exn:fail? (lambda (exn)
                               (unless (regexp-match? rx (exn-message exn))
                                 (error "wrong error" (exn-message exn)))
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

"syntax-local-value"
(eval-expression
 '(let-values ([(x) 1])
   (letrec-syntaxes+values
    ([(x-id) (quote-syntax x)])
    ()
    (letrec-syntaxes+values
     ([(m) (lambda (stx) (syntax-local-value (quote-syntax x-id)))])
     ()
     (let-values ([(x) 2])
       (m)))))
 #:check 1)

"local-expand"
(eval-expression
 '(let-values ([(x) 10])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (quote-syntax (something x)))])
    ()
    (letrec-syntaxes+values
     ([(n) (lambda (stx) (car
                     (cdr
                      (syntax-e
                       (local-expand (car (cdr (syntax-e stx)))
                                     'expression
                                     (list (quote-syntax #%app)))))))])
     ()
     (let-values ([(x) 20])
       (n (m))))))
 #:check 10)

"local-expand-expression"
(eval-expression
 '(letrec-syntaxes+values
   ([(m) (lambda (stx) (quote-syntax 5))])
   ()
   (letrec-syntaxes+values
    ([(n) (lambda (stx)
            (let-values ([(expr already)
                          (syntax-local-expand-expression (car (cdr (syntax-e stx))))])
              (datum->syntax
               (quote-syntax here)
               (list (quote-syntax +)
                     (quote-syntax 1)
                     already))))])
    ()
    (n (m))))
 #:check 6)

(check-error
 (eval-expression
  '(letrec-syntaxes+values
    ([(m) (lambda (stx) (quote-syntax 5))])
    ()
    (letrec-syntaxes+values
     ([(n) (lambda (stx)
             (let-values ([(expr already)
                           (syntax-local-expand-expression (car (cdr (syntax-e stx))))])
               (datum->syntax
                #f
                (list
                 (quote-syntax let-values)
                 (list (list (list (quote-syntax x)) (quote-syntax 1)))
                 already))))])
     ()
     (n (m)))))
 #rx"expanded syntax not in its original lexical context")

"internal definition context"
(eval-expression
 '(let-values ([(x) 10])
   (letrec-syntaxes+values
    ([(m) (lambda (stx)
            (let-values ([(id) (car (cdr (syntax-e stx)))]
                         [(id2) (car (cdr (cdr (syntax-e stx))))]
                         [(intdef) (syntax-local-make-definition-context)])
              (syntax-local-bind-syntaxes (list id)
                                          (quote-syntax (lambda (stx) (quote-syntax 5)))
                                          intdef)
              (syntax-local-bind-syntaxes (list id2)
                                          #f
                                          intdef)
              (datum->syntax
               (quote-syntax here)
               (list (quote-syntax let-values)
                     (list (list (list
                                  (let-values ([(id2-by-expand)
                                                (car
                                                 (cdr
                                                  (syntax-e (local-expand (datum->syntax
                                                                           #f
                                                                           (list (quote-syntax quote)
                                                                                 id2))
                                                                          (list 'intdef)
                                                                          (list (quote-syntax quote))
                                                                          intdef))))]
                                               [(id2-by-intro)
                                                (internal-definition-context-introduce
                                                 intdef
                                                 id2)]
                                               [(flip) (make-syntax-introducer)])
                                    (if (bound-identifier=? id2-by-expand id2-by-intro)
                                        (let-values ([(delta)
                                                      (make-syntax-delta-introducer
                                                       (flip (quote-syntax here))
                                                       (quote-syntax here))])
                                          (syntax-local-identifier-as-binding
                                           (delta (flip id2-by-intro) 'remove)))
                                        (error "should have been the same"))))
                                 7))
                     (local-expand (datum->syntax
                                    (quote-syntax here)
                                    (list (quote-syntax +)
                                          (list id)
                                          id2))
                                   (list 'intdef)
                                   (list)
                                   intdef)))))])
                          
    ()
    (m x y)))
 #:check 12)

"set! transformer"
(eval-expression
 '(let-values ([(real-one) 1]
               [(check-one) (lambda (v)
                              (if (equal? v 1)
                                  'ok
                                  'oops))])
   (letrec-syntaxes+values
    ([(one)
      (make-set!-transformer
       (lambda (stx)
         (if (pair? (syntax-e stx))
             (if (free-identifier=? (car (syntax-e stx))
                                    (quote-syntax set!))
                 (datum->syntax
                  (quote-syntax here)
                  (list (quote-syntax check-one)
                        (car (cdr (cdr (syntax-e stx))))))
                 (datum->syntax
                  stx
                  (cons
                   (quote-syntax list)
                   (cons
                    (quote-syntax real-one)
                    (cdr (syntax-e stx))))))
             (quote-syntax real-one))))])
    ()
    (list one
          (set! one 5)
          (set! one 1)
          (one 8))))
 #:check (list 1 'oops 'ok '(1 8)))

"rename transformer"
(eval-expression
 '(let-values ([(f) (lambda (v) (+ v 1))])
   (letrec-syntaxes+values
    ([(g) (make-rename-transformer (quote-syntax f))])
    ()
    (list (let-values ([(h) g]) (h 0))
          (g 1)
          (begin
            (set! g 3)
            f)
          (letrec-syntaxes+values
           ([(f-id) (quote-syntax f)])
           ()
           (letrec-syntaxes+values
            ([(g-id) (make-rename-transformer (quote-syntax f-id))])
            ()
            (letrec-syntaxes+values
             ([(m) (lambda (stx) (syntax-local-value (quote-syntax g-id)))])
             ()
             (+ 1 (m))))))))
 #:check (list 1 2 3 4))

"lifts in transformer; same number twice"
(eval-expression '(letrec-syntaxes+values
                   ([(n) (lambda (stx)
                           (letrec-syntaxes+values
                            ([(m) (lambda (stx)
                                    (datum->syntax
                                     (quote-syntax here)
                                     (list (quote-syntax println)
                                           (syntax-local-lift-expression
                                            (quote-syntax (random))))))])
                            ()
                            (datum->syntax (quote-syntax here)
                                           (m))))])
                   ()
                   (list (n) (n))))

"local-expand/capture-lifts"
(eval-expression '(letrec-syntaxes+values
                   ([(m) (lambda (stx)
                           (syntax-local-lift-expression (quote-syntax 1)))])
                   ()
                   (letrec-syntaxes+values
                    ([(n) (lambda (stx)
                            (datum->syntax
                             (quote-syntax here)
                             (list (quote-syntax quote)
                                   (local-expand/capture-lifts
                                    (quote-syntax (m))
                                    'expression
                                    '()))))])
                    ()
                    (let-values ([(x) (n)])
                      (list (car x)
                            (car (car (cdr x)))
                            (car (cdr (cdr (car (cdr x)))))))))
                 #:check '(begin define-values 1))

"get shadower"
(eval-expression
 '(let-values ([(x) 1])
   (letrec-syntaxes+values
    ([(m)
      (lambda (stx)
        (datum->syntax
         #f
         (list (quote-syntax let-values)
               (list
                (list
                 (list (syntax-local-introduce
                        (syntax-local-get-shadower (quote-syntax x))))
                 (quote-syntax 2)))
               (car (cdr (syntax-e stx))))))])
    ()
    (let-values ([(x) 3])
      (m x))))
 #:check 2)

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
                           (define-values (posn make-posn struct:posn posn? 
                                                posn-x posn-y
                                                set-posn-x! set-posn-y!)
                             (values 1 2 3 4 5 6 7 8))
                           (#%provide (prefix-all-defined def:)
                                      (struct posn (x y))) 
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
                           (println (f 5))
                           
                           (define-syntaxes (define-x)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (list (quote-syntax begin-for-syntax)
                                      (list (quote-syntax define-values)
                                            (list (car (cdr (syntax-e stx))))
                                            (quote-syntax 'ct-5))))))
                           (define-x ct-5)
                           (define-syntaxes (ct-five)
                             (lambda (stx)
                               (datum->syntax (quote-syntax here)
                                              (list (quote-syntax quote)
                                                    ct-5))))
                           (println (ct-five))))

(check-print
 (namespace-require ''with-use-site-scope demo-ns)
 5
 'ct-5)

(eval-module-declaration '(module definition-shadows-initial-require '#%core
                           (#%require (rename '#%core orig:list list))
                           (#%provide list)
                           (define-values (list)
                             (lambda (a b)
                               (println a)
                               (orig:list a b)))))

(eval-module-declaration '(module definition-shadows-plain-require '#%core
                           (#%require '#%core)
                           (#%provide map)
                           (define-values (map)
                             (lambda (f l)
                               (if (null? l)
                                   '()
                                   (cons (car l) ; don't use `f`
                                         (map f (cdr l))))))))

(eval-module-declaration '(module require-shadows-initial-require '#%core
                           (#%require 'definition-shadows-initial-require
                                      'definition-shadows-plain-require)
                           (println (map pair? (list 'a 'b)))))

(check-print
 (namespace-require ''require-shadows-initial-require demo-ns)
 'a
 '(a b))

(check-error
 (eval-module-declaration '(module m '#%core
                            (#%require '#%core 
                                       'definition-shadows-initial-require)))
 #rx"already required")

(check-error
 (eval-module-declaration '(module m '#%core
                            (define-values (list) 5)
                            (#%require '#%core)))
 #rx"already defined")

;; ----------------------------------------

(check-print
 (eval-module-declaration '(module forward-reference-in-begin-for-syntax '#%core
                            (#%require (for-syntax '#%core))
                            (begin-for-syntax
                              (define-values (even) (lambda () odd)))
                            (begin-for-syntax
                              (define-values (odd) (lambda () even)))
                            (begin-for-syntax
                              (define-values (assign-later!) (lambda () (set! later also-later))))
                            (begin-for-syntax
                             (define-values (later) 5)
                             (define-values (also-later) 6)
                             (assign-later!)
                             (println later))))
 6)

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

(eval-module-declaration '(module intro-printed-submodule '#%core
                           (#%require (for-syntax '#%core))
                           (#%provide m)
                           (define-syntaxes (m)
                             (lambda (stx)
                               (quote-syntax
                                (module sub 'printing-mb
                                  (+ 5 6)
                                  (+ 7 8)))))))

(eval-module-declaration '(module printed-submodule '#%core
                           (#%require 'intro-printed-submodule)
                           (m)))

(check-print
 (namespace-require '(submod 'printed-submodule sub) demo-ns)
 11
 15)

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

(eval-module-declaration '(module used-by-shifted-submodule '#%core
                           (define-values (x) 'x)
                           (#%provide x)))

(eval-module-declaration '(module with-shifted-pre-submodule '#%core
                           (#%require (for-syntax '#%core))
                           (begin-for-syntax
                             (module xa '#%core
                               (#%require 'used-by-shifted-submodule)
                               (#%provide xa)
                               (define-values (xa) x)))
                           (#%require (submod "." xa))
                           (println xa)))

(check-print
 (namespace-require ''with-shifted-pre-submodule demo-ns)
 'x)

(eval-module-declaration '(module with-shifted-#f-submodule '#%core
                           (#%require (for-syntax '#%core
                                                  'used-by-shifted-submodule))
                           (define-values (d) 'd)
                           (begin-for-syntax
                             (define-values (d-stx) (quote-syntax d))
                             (module* d #f
                               (#%provide get-d-stx)
                               x
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

;; ----------------------------------------
;; syntax-local-value of module binding

(eval-module-declaration '(module define-non-transformer '#%core
                           (#%require (for-syntax '#%core))
                           (#%provide car-id)
                           (define-syntaxes (car-id) (quote-syntax car))))

(eval-module-declaration '(module use-non-transformer '#%core
                           (#%require (for-syntax '#%core)
                                      'define-non-transformer)
                           (define-syntaxes (m)
                             (lambda (stx) (syntax-local-value (quote-syntax car-id))))
                           (println ((m) '(1 2)))))

(check-print
 (namespace-require ''use-non-transformer demo-ns)
 1)

;; ----------------------------------------
;; syntax-local-lift-{expression,module}, etc.

(eval-module-declaration '(module lifts '#%core
                           (#%require (for-syntax '#%core))
                           (module pre '#%core
                             (#%provide pre)
                             (define-values (pre) 'pre))
                           (define-syntaxes (m)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (list (quote-syntax println)
                                      (syntax-local-lift-expression
                                       (quote-syntax (+ 1 2)))))))
                           (m)
                           (define-syntaxes (n)
                             (lambda (stx)
                               (syntax-local-lift-module
                                (quote-syntax (module sub '#%core
                                                (#%provide sub)
                                                (define-values (sub) 'sub))))
                               (syntax-local-lift-module
                                (quote-syntax (module* main #f
                                                (println x))))
                               (syntax-local-lift-module-end-declaration
                                (quote-syntax (define-values (done) 'done)))
                               (syntax-local-lift-provide
                                (quote-syntax done))
                               (let-values ([(pre-id) (syntax-local-lift-require
                                                       (quote-syntax (submod "." pre))
                                                       (quote-syntax pre))])
                                 (datum->syntax
                                  (quote-syntax here)
                                  (list
                                   (quote-syntax begin)
                                   (list (quote-syntax println) pre-id)
                                   (quote-syntax (#%require (submod "." sub)))
                                   (quote-syntax (println sub)))))))
                           (n)
                           (define-values (x) '*)
                           (define-syntaxes (as-expr)
                             (lambda (stx)
                               ;; (syntax-local-lift-module-end-declaration
                               ;;  (quote-syntax (define-values (fail) 'this-wont-work)))
                               (syntax-local-lift-module-end-declaration
                                (quote-syntax (println 'end)))
                               (quote-syntax (void))))
                           (list (as-expr))))

(check-print
 (namespace-require '(submod 'lifts main) demo-ns)
 3
 'pre
 'sub
 'end
 '*)

(eval-module-declaration '(module use-lifted-provide '#%core
                           (#%require 'lifts)
                           (println done)))

(check-print
 (namespace-require ''use-lifted-provide demo-ns)
 'done)

;; ----------------------------------------
;; `local-transformer-expand`

(eval-module-declaration '(module local-transformer-expand '#%core
                           (#%require (for-syntax '#%core))
                           (define-syntaxes (m)
                             (lambda (stx)
                               (datum->syntax
                                #f
                                (list
                                 (quote-syntax letrec-syntaxes+values)
                                 (list
                                  (list (list (car (cdr (syntax-e stx))))
                                        (local-transformer-expand
                                         (car (cdr (cdr (syntax-e stx))))
                                         'expression
                                         '())))
                                 (list)
                                 (car (cdr (cdr (cdr (syntax-e stx)))))))))
                           (begin-for-syntax
                             (#%require (for-syntax '#%core))
                             (define-syntaxes (tm)
                               (lambda (stx)
                                 (quote-syntax (quote-syntax 'local-trans)))))
                           (println (m p (lambda (stx) (tm)) (p)))))

(check-print
 (namespace-require ''local-transformer-expand demo-ns)
 'local-trans)

;; ----------------------------------------
;; `expand` in `#%provide`

(eval-module-declaration '(module expand-provide '#%core
                           (#%require (for-syntax '#%core))
                           (module sub '#%core
                             (#%provide a-sub b-sub)
                             (define-values (a-sub) 'a-sub)
                             (define-values (b-sub) 'b-sub))
                           (#%require (submod "." sub))
                           (define-values (a-here) 'a-here)
                           (define-values (b-here) 'b-here)
                           (define-syntaxes (all-a)
                             (lambda (stx)
                               (let-values ([(here) (syntax-local-module-defined-identifiers)]
                                            [(there) (syntax-local-module-required-identifiers
                                                      '(submod "." sub)
                                                      0)]
                                            [(keep-a) (lambda (id)
                                                        (regexp-match? #rx"^a"
                                                                       (symbol->string
                                                                        (syntax-e id))))])
                                 (datum->syntax
                                  #f
                                  (cons
                                   (quote-syntax begin)
                                   (append
                                    (filter keep-a (hash-ref here 0))
                                    (filter keep-a (cdr (assv 0 there)))))))))
                           (#%provide (expand (all-a)))))

(eval-module-declaration '(module use-expand-provide '#%core
                           (#%require 'expand-provide)
                           (println (list a-sub a-here))))

(check-print
 (namespace-require ''use-expand-provide demo-ns)
 (list 'a-sub 'a-here))

;; ----------------------------------------
;; cross-phase persistent declaration

(eval-module-declaration '(module cross-phase-persistent '#%core
                           (#%declare #:cross-phase-persistent)
                           (#%require '#%core)
                           (#%provide gen)
                           (define-values (gen) (gensym "g"))
                           (module ignored '#%core)
                           (module* also-ignored '#%core)
                           (begin
                             (define-values (y) (lambda () (error "anything")))
                             (define-values (x) (case-lambda
                                                  [() (error "anything")]
                                                  [(x) (set! x x)])))
                           (define-values (z) (list
                                               #t
                                               (cons 1 2)
                                               "string"
                                               #"bytes"
                                               'symbol
                                               (gensym)
                                               (string->uninterned-symbol "u")))))

(eval-module-declaration '(module use-cross-phase-persistent '#%core
                           (#%require 'cross-phase-persistent
                                      (for-syntax '#%core
                                                  'cross-phase-persistent))
                           (define-syntaxes (ct-gen)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (list (quote-syntax quote)
                                      gen))))
                           (println (equal? gen (ct-gen)))))

(check-print
 (namespace-require ''use-cross-phase-persistent demo-ns)
 #t)
