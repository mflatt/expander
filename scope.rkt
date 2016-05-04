#lang racket/base
(require racket/set
         "syntax.rkt")

(provide new-scope
         add-scope
         remove-scope
         remove-scopes
         flip-scope
         
         add-binding-in-scopes!
         add-binding!
         resolve
         
         bound-identifier=?)

;; A scope represents a distinct "dimension" of binding. We can attach
;; the bindings for a set of scopes to an arbitrary scope in the set;
;; we pick the most recently allocated scope to make a binding search
;; faster and to improve GC, since non-nested binding contexts will
;; generally not share a most-recent scope.

(struct scope (id          ; internal scope identity; used for sorting
               bindings)   ; sym -> scope-set -> binding
        ;; Custom printer:
        #:property prop:custom-write
        (lambda (sc port mode)
          (write-string "#<scope:" port)
          (display (scope-id sc) port)
          (write-string ">" port)))

;; Each new scope increments the counter, so we can check whether one
;; scope is newer than another.
(define id-counter 0)
(define (new-scope-id!)
  (set! id-counter (add1 id-counter))
  id-counter)
  
(define (make-bindings)
  (make-hasheq))

(define (new-scope)
  (scope (new-scope-id!) (make-bindings)))

(define (scope>? sc1 sc2)
  ((scope-id sc1) . > . (scope-id sc2)))

;; Add, remove, or flip a scope --- recurs to nested syntax
(define (apply-scope s sc op)
  (cond
   [(syntax? s) (struct-copy syntax s
                             [e (apply-scope (syntax-e s) sc op)]
                             [scopes (op (syntax-scopes s) sc)])]
   [(pair? s) (cons (apply-scope (car s) sc op)
                    (apply-scope (cdr s) sc op))]
   [else s]))

(define (add-scope s sc)
  (apply-scope s sc set-add))

(define (remove-scope s sc)
  (apply-scope s sc set-remove))

(define (remove-scopes s scs)
  (for/fold ([s s]) ([sc (in-list scs)])
    (remove-scope s sc)))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (apply-scope s sc set-flip))

;; ----------------------------------------

(define (add-binding-in-scopes! scopes sym binding)
  (when (set-empty? scopes)
    (error "cannot bind in empty scope set"))
  (define max-sc (for/fold ([max-sc (set-first scopes)]) ([sc (in-set scopes)])
                   (if (scope>? sc max-sc)
                       sc
                       max-sc)))
  (define bindings (scope-bindings max-sc))
  (define sym-bindings (or (hash-ref bindings sym #f)
                           (let ([h (make-hash)])
                             (hash-set! bindings sym h)
                             h)))
  (hash-set! sym-bindings scopes binding))

(define (add-binding! id binding)
  (add-binding-in-scopes! (syntax-scopes id) (syntax-e id) binding))

(define (resolve s #:exactly? [exactly? #f])
  (unless (identifier? s)
    (raise-argument-error 'resolve "identifier?" s))
  (define scopes (syntax-scopes s))
  (define sym (syntax-e s))
  (define candidates
    (for*/list ([sc (in-set scopes)]
                [bindings (in-value (hash-ref (scope-bindings sc) sym #f))]
                #:when bindings
                [(b-scopes binding) (in-hash bindings)]
                #:when (subset? b-scopes scopes))
      (cons b-scopes binding)))
  (define max-candidate
    (and (pair? candidates)
         (for/fold ([max-c (car candidates)]) ([c (in-list (cdr candidates))])
           (if ((set-count (car c)) . > . (set-count (car max-c)))
               c
               max-c))))
  (cond
   [max-candidate
    (for ([c (in-list candidates)])
      (unless (subset? (car c) (car max-candidate))
        (error "ambiguous:" s scopes)))
    (and (or (not exactly?)
             (equal? (set-count scopes)
                     (set-count (car max-candidate))))
         (cdr max-candidate))]
   [else #f]))

;; ----------------------------------------

(define (bound-identifier=? a b)
  (and (eq? (syntax-e a)
            (syntax-e b))
       (equal? (syntax-scopes a)
               (syntax-scopes b))))
