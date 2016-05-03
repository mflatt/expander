#lang racket/base
(require racket/set
         "syntax.rkt"
         "phase.rkt")

(provide new-scope
         new-multi-scope
         shift-multi-scope
         add-scope
         remove-scope
         flip-scope
         
         syntax-shift-phase-level
         
         add-binding-in-scopes!
         add-binding!
         resolve
         
         bound-identifier=?)

(struct scope (id          ; internal scope identity; used for sorting
               bindings)   ; sym -> scope-set -> binding
        ;; Custom printer:
        #:property prop:custom-write
        (lambda (sc port mode)
          (write-string "#<scope:" port)
          (display (scope-id sc) port)
          (write-string ">" port)))
(struct representative-scope scope (owner   ; a multi-scope for which this one is a phase-specific identity
                                    phase)) ; phase of this scope
(struct multi-scope (scopes)) ; phase -> representative-scope
(struct shifted-multi-scope (phase        ; phase shift applies to all scopes in multi-scope
                             multi-scope) ; a multi-scope
        #:transparent)

(define id-counter 0)
(define (new-scope-id!)
  (set! id-counter (add1 id-counter))
  id-counter)
  
(define (make-bindings)
  (make-hasheq))

(define (new-scope)
  (scope (new-scope-id!) (make-bindings)))

(define (new-multi-scope [name (gensym)])
  (shifted-multi-scope 0 (multi-scope (make-hasheqv))))

(define (shift-multi-scope sms delta)
  (if (zero? delta)
      sms
      (shifted-multi-scope (phase+ delta (shifted-multi-scope-phase sms))
                           (shifted-multi-scope-multi-scope sms))))

(define (multi-scope-to-scope-at-phase ms phase)
  ;; Get the identity of `ms` at phase`
  (or (hash-ref (multi-scope-scopes ms) phase #f)
      (let ([s (representative-scope (new-scope-id!) (make-bindings) ms phase)])
        (hash-set! (multi-scope-scopes ms) phase s)
        s)))

(define (scope>? sc1 sc2)
  ((scope-id sc1) . > . (scope-id sc2)))

;; FIXME: Adding, removing, or flipping a scope currently recurs
;; through a syntax object eagerly, but it should be lazy
(define (apply-scope s sc op)
  (cond
   [(syntax? s) (struct-copy syntax s
                             [e (apply-scope (syntax-e s) sc op)]
                             [scopes
                              (if (shifted-multi-scope? sc)
                                  (syntax-scopes s)
                                  (op (syntax-scopes s) sc))]
                             [shifted-multi-scopes
                              (if (shifted-multi-scope? sc)
                                  (op (syntax-shifted-multi-scopes s) sc)
                                  (syntax-shifted-multi-scopes s))])]
   [(pair? s) (cons (apply-scope (car s) sc op)
                    (apply-scope (cdr s) sc op))]
   [else s]))

;; When a representative-scope is manipulated, we want to
;; manipulate the multi scope, instead (at a particular
;; phase shift)
(define (generalize-scope sc)
  (if (representative-scope? sc)
      (shifted-multi-scope (representative-scope-phase sc)
                           (representative-scope-owner sc))
      sc))

(define (add-scope s sc)
  (apply-scope s (generalize-scope sc) set-add))

(define (remove-scope s sc)
  (apply-scope s (generalize-scope sc) set-remove))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (apply-scope s (generalize-scope sc) set-flip))

;; FIXME: this should be lazy, too
(define (syntax-shift-phase-level s phase)
  (if (zero? phase) 
      s
      (let loop ([s s])
        (cond
         [(syntax? s) (struct-copy syntax s
                                   [e (loop (syntax-e s))]
                                   [shifted-multi-scopes
                                    (for/set ([sms (in-set (syntax-shifted-multi-scopes s))])
                                      (shift-multi-scope sms phase))])]
         [(pair? s) (cons (loop (car s))
                          (loop (cdr s)))]
         [else s]))))

;; ----------------------------------------

(define (syntax-scope-set s phase)
  (for/fold ([scopes (syntax-scopes s)]) ([sms (in-set (syntax-shifted-multi-scopes s))])
    (set-add scopes (multi-scope-to-scope-at-phase (shifted-multi-scope-multi-scope sms)
                                                   (phase- (shifted-multi-scope-phase sms)
                                                           phase)))))

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

(define (add-binding! id binding phase)
  (add-binding-in-scopes! (syntax-scope-set id phase) (syntax-e id) binding))

(define (resolve s phase #:exactly? [exactly? #f])
  (unless (identifier? s)
    (raise-argument-error 'resolve "identifier?" s))
  (unless (phase? phase)
    (raise-argument-error 'resolve "phase?" phase))
  (define scopes (syntax-scope-set s phase))
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

(define (bound-identifier=? a b phase)
  (unless (identifier? a)
    (raise-argument-error 'bound-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'bound-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? "phase?" phase))
  (and (eq? (syntax-e a)
            (syntax-e b))
       (equal? (syntax-scope-set a phase)
               (syntax-scope-set b phase))))
