#lang racket/base
(require racket/set
         "syntax.rkt"
         "scope.rkt"
         "phase.rkt"
         "namespace.rkt")

(provide
 (struct-out module-binding)
 (struct-out local-binding)

 (struct-out core-form)
 (struct-out local-transformer)
 
 transformer?
 variable?
 
 empty-env
 env-extend
 
 binding-lookup
 
 free-identifier=?
 
 add-local-binding!
 
 core-form-sym)
 
(define (transformer? t) (procedure? t))
(define (variable? t) (eq? t 'variable))

;; see `identifier-binding` docs for information about these fields:
(struct module-binding (module phase sym
                         nominal-module nominal-phase nominal-sym
                         nominal-import-phase)
        #:transparent)
(struct local-binding (key))

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
                                           (- phase
                                              (module-binding-phase b))))
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

(define (free-identifier=? a b phase)
  (unless (identifier? a)
    (raise-argument-error 'free-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'free-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'free-identifier=? "phase?" phase))
  (define ab (resolve a phase))
  (define bb (resolve a phase))
  (cond
   [(module-binding? ab)
    (and (module-binding? bb)
         (eq? (module-binding-sym ab)
              (module-binding-sym bb))
         (eqv? (module-binding-phase ab)
               (module-binding-phase bb))
         (equal? (module-binding-module ab)
                 (module-binding-module bb)))]
   [(local-binding? ab)
    (and (local-binding? bb)
         (eq? (local-binding-key ab)
              (local-binding-key bb)))]))

;; ----------------------------------------

(define (add-local-binding! id phase)
  (define key (gensym))
  (add-binding! id (local-binding key) phase)
  key)

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
