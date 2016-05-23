#lang racket/base
(require "set.rkt"
         "serialize-property.rkt"
         "full-binding.rkt")

(provide make-module-binding
         module-binding-update
         module-binding?
         
         module-binding-module
         module-binding-phase
         module-binding-sym
         module-binding-nominal-module
         module-binding-nominal-phase
         module-binding-nominal-sym
         module-binding-nominal-require-phase
         
         deserialize-full-module-binding
         deserialize-simple-module-binding)

;; ----------------------------------------

(define (make-module-binding module phase sym
                             #:wrt [wrt-sym sym]
                             #:nominal-module [nominal-module module]
                             #:nominal-phase [nominal-phase phase]
                             #:nominal-sym [nominal-sym sym]
                             #:nominal-require-phase [nominal-require-phase 0]
                             #:frame-id [frame-id #f]
                             #:free=id [free=id #f])
  (cond
   [(or frame-id
        free=id
        (not (and (eq? nominal-module module)
                  (eqv? nominal-phase phase)
                  (eq? nominal-sym sym)
                  (eqv? nominal-require-phase 0))))
    (full-module-binding frame-id
                         free=id
                         module phase sym
                         nominal-module nominal-phase nominal-sym
                         nominal-require-phase)]
   [else
    (simple-module-binding module phase sym)]))

(define (module-binding-update b
                               #:module [module (module-binding-module b)]
                               #:phase [phase (module-binding-phase b)]
                               #:sym [sym (module-binding-sym b)]
                               #:nominal-module [nominal-module (module-binding-nominal-module b)]
                               #:nominal-phase [nominal-phase (module-binding-nominal-phase b)]
                               #:nominal-sym [nominal-sym (module-binding-nominal-sym b)]
                               #:nominal-require-phase [nominal-require-phase (module-binding-nominal-require-phase b)]
                               #:frame-id [frame-id (binding-frame-id b)]
                               #:free=id [free=id (binding-free=id b)])
  (make-module-binding module phase sym
                       #:nominal-module nominal-module
                       #:nominal-phase nominal-phase
                       #:nominal-sym nominal-sym
                       #:nominal-require-phase nominal-require-phase
                       #:frame-id frame-id
                       #:free=id free=id))

(define (module-binding? b)
  ;; must not overlap with `local-binding?`
  (or (simple-module-binding? b)
      (full-module-binding? b)))

;; See `identifier-binding` docs for information about these fields:
(struct full-module-binding full-binding (module phase sym
                                           nominal-module nominal-phase nominal-sym
                                           nominal-require-phase)
        #:transparent
        #:property prop:serialize
        (lambda (b ser state)
          ;; Dropping the frame id may simplify the representation:
          (define simplified-b
            (if (full-binding-frame-id b)
                (module-binding-update b #:frame-id #f)
                b))
          (cond
           [(full-module-binding? simplified-b)
            `(deserialize-full-module-binding
              ,(ser (full-module-binding-module b))
              ,(ser (full-module-binding-sym b))
              ,(ser (full-module-binding-phase b))
              ,(ser (full-module-binding-nominal-module b))
              ,(ser (full-module-binding-nominal-phase b))
              ,(ser (full-module-binding-nominal-sym b))
              ,(ser (full-module-binding-nominal-require-phase b))
              ,(ser (full-binding-free=id b)))]
           [else
            (ser simplified-b)])))

(struct simple-module-binding (module phase sym)
        #:transparent
        #:property prop:serialize
        (lambda (b ser state)
          `(deserialize-simple-module-binding
            ,(ser (simple-module-binding-module b))
            ,(ser (simple-module-binding-sym b))
            ,(ser (simple-module-binding-phase b)))))

(define (deserialize-full-module-binding module sym phase
                                         nominal-module
                                         nominal-phase
                                         nominal-sym
                                         nominal-require-phase
                                         free=id)
  (make-module-binding module phase sym
                       #:nominal-module nominal-module
                       #:nominal-phase nominal-phase
                       #:nominal-sym nominal-sym
                       #:nominal-require-phase nominal-require-phase
                       #:free=id free=id))

(define (deserialize-simple-module-binding module sym phase)
  (simple-module-binding module phase sym))

;; ----------------------------------------

(define (module-binding-module b)
  (if (simple-module-binding? b)
      (simple-module-binding-module b)
      (full-module-binding-module b)))

(define (module-binding-phase b)
  (if (simple-module-binding? b)
      (simple-module-binding-phase b)
      (full-module-binding-phase b)))

(define (module-binding-sym b)
  (if (simple-module-binding? b)
      (simple-module-binding-sym b)
      (full-module-binding-sym b)))

(define (module-binding-nominal-module b)
  (if (simple-module-binding? b)
      (simple-module-binding-module b)
      (full-module-binding-nominal-module b)))
       
(define (module-binding-nominal-phase b)
  (if (simple-module-binding? b)
      (simple-module-binding-phase b)
      (full-module-binding-nominal-phase b)))

(define (module-binding-nominal-sym b)
  (if (simple-module-binding? b)
      (simple-module-binding-sym b)
      (full-module-binding-nominal-sym b)))

(define (module-binding-nominal-require-phase b)
  (if (simple-module-binding? b)
      0
      (full-module-binding-nominal-require-phase b)))
