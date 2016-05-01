#lang racket/base
(require racket/set
         "stx.rkt")

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
         
         phase?
         phase+
         bound-identifier=?
         free-identifier=?)

(define id-counter 0)

(struct scope (id bindings))
(struct pseudo-scope scope (owner phase))
(struct multi-scope (scopes))
(struct shifted-multi-scope (phase multi-scope) #:transparent)

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
  (shifted-multi-scope (+ delta (shifted-multi-scope-phase sms))
                       (shifted-multi-scope-multi-scope sms)))

(define (multi-scope-to-scope-at-phase ms phase)
  (or (hash-ref (multi-scope-scopes ms) phase #f)
      (let ([s (pseudo-scope (new-scope-id!) (make-bindings) ms phase)])
        (hash-set! (multi-scope-scopes ms) phase s)
        s)))

(define (scope>? sc1 sc2)
  ((scope-id sc1) . > . (scope-id sc2)))

(define (apply-scope s sc op)
  (cond
   [(stx? s) (stx (apply-scope (stx-e s) sc op)
                  (if (shifted-multi-scope? sc)
                      (stx-scopes s)
                      (op (stx-scopes s) sc))
                  (if (shifted-multi-scope? sc)
                      (op (stx-shifted-multi-scopes s) sc)
                      (stx-shifted-multi-scopes s))
                  (stx-srcloc s)
                  (stx-props s))]
   [(pair? s) (cons (apply-scope (car s) sc op)
                    (apply-scope (cdr s) sc op))]
   [else s]))

(define (generalize-scope sc)
  (if (pseudo-scope? sc)
      (shifted-multi-scope (pseudo-scope-phase sc)
                           (pseudo-scope-owner sc))
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

(define (syntax-shift-phase-level s phase)
  (if (zero? phase) 
      s
      (let loop ([s s])
        (cond
         [(stx? s) (struct-copy stx s
                                [e (loop (stx-e s))]
                                [shifted-multi-scopes
                                 (for/set ([sms (in-set (stx-shifted-multi-scopes s))])
                                   (shifted-multi-scope (+ phase (shifted-multi-scope-phase sms))
                                                        (shifted-multi-scope-multi-scope sms)))])]
         [(pair? s) (cons (loop (car s))
                          (loop (cdr s)))]
         [else s]))))

;; ----------------------------------------

(define (stx-scope-set s phase)
  (for/fold ([scopes (stx-scopes s)]) ([sms (in-set (stx-shifted-multi-scopes s))])
    (set-add scopes (multi-scope-to-scope-at-phase (shifted-multi-scope-multi-scope sms)
                                                   (- (shifted-multi-scope-phase sms) phase)))))

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
  (add-binding-in-scopes! (stx-scope-set id phase) (syntax-e id) binding))

(define (resolve s phase)
  (unless (identifier? s)
    (raise-argument-error 'resolve "identifier?" s))
  (unless (phase? phase)
    (raise-argument-error 'resolve "phase?" phase))
  (define scopes (stx-scope-set s phase))
  (define sym (stx-e s))
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
        (error "ambiguous")))
    (cdr max-candidate)]
   [else #f]))

;; ----------------------------------------

(define (phase? v)
  (or (not v)
      (exact-integer? v)))

(define (phase+ a b)
  (and a b (+ a b)))

(define (bound-identifier=? a b phase)
  (unless (identifier? a)
    (raise-argument-error 'bound-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'bound-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? "phase?" phase))
  (and (eq? (syntax-e a)
            (syntax-e b))
       (equal? (stx-scope-set a phase)
               (stx-scope-set b phase))))

(define (free-identifier=? a b phase)
  (unless (identifier? a)
    (raise-argument-error 'free-identifier=? "identifier?" a))
  (unless (identifier? b)
    (raise-argument-error 'free-identifier=? "identifier?" b))
  (unless (phase? phase)
    (raise-argument-error 'free-identifier=? "phase?" phase))
  (equal? (resolve a phase)
          (resolve b phase)))
