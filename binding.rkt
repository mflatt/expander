#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         "namespace.rkt")

(provide
 (struct-out module-binding)
 (struct-out local-binding)
 (struct-out top-level-binding)

 (struct-out core-form)
 (struct-out local-transformer)
 
 transformer?
 variable?
 
 empty-env
 env-extend
 
 binding-lookup
 
 core-form-sym)
 
(define (transformer? t) (procedure? t))
(define (variable? t) (eq? t 'variable))

(struct module-binding (module phase sym
                         nominal-module nominal-phase nominal-sym
                         nominal-import-phase)
        #:transparent)
(struct local-binding (key))
(struct top-level-binding (module phase sym))

(struct core-form (expander) #:transparent)
(struct local-transformer (value))

(define empty-env #hasheq())

(define (env-extend env key val)
  (hash-set env key val))

(define (binding-lookup b env ns phase id)
  (cond
   [(module-binding? b)
    (define m (namespace->module-namespace ns
                                           (module-binding-module b)
                                           (module-binding-nominal-import-phase b)))
    (lookup-in-namespace m (module-binding-phase b) (module-binding-sym b) id)]
   [(local-binding? b)
    (define l (hash-ref env
                        (local-binding-key b)
                        #f))
    (cond
     [(not l) (error "identifier used out of context:" id)]
     [(local-transformer? l)
      (define tr (local-transformer-value l))
      (cond
       [(transformer? tr) tr]
       [else (lambda (s) (error "bad syntax:" id))])]
     [else 'variable])]
   [(top-level-binding? b)
    (lookup-in-namespace ns
                         (top-level-binding-phase b) 
                         (top-level-binding-sym b)
                         id)]
   [else (error "unknown binding for lookup:" b)]))

(define not-found (gensym))

(define (lookup-in-namespace ns phase def id)
  (define tr (namespace-get-transformer ns phase def not-found))
  (cond
   [(eq? tr not-found) 'variable]
   [(transformer? tr) tr]
   [(core-form? tr) tr]
   [else (lambda (s) (error "bad syntax: " id))]))

;; ----------------------------------------

(define (core-form-sym s phase)
  (and (pair? (syntax-e s))
       (let ()
         (define id (car (syntax-e s)))
         (and (identifier? id)
              (let ()
                (define b (resolve id phase))
                (and (module-binding? b)
                     (eq? '#%core (module-binding-module b))
                     (module-binding-sym b)))))))
