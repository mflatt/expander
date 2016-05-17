#lang racket/base
(require racket/set
         racket/serialize
         "syntax.rkt"
         "phase.rkt")

(provide new-scope
         new-multi-scope
         add-scope
         add-scopes
         remove-scope
         remove-scopes
         flip-scope
         flip-scopes
         
         syntax-scope-set
         
         syntax-shift-phase-level

         add-binding-in-scopes!
         add-binding!
         resolve
         
         bound-identifier=?)

(module+ for-debug
  (provide (struct-out scope)))

;; A scope represents a distinct "dimension" of binding. We can attach
;; the bindings for a set of scopes to an arbitrary scope in the set;
;; we pick the most recently allocated scope to make a binding search
;; faster and to improve GC, since non-nested binding contexts will
;; generally not share a most-recent scope.

(serializable-struct scope (id          ; internal scope identity; used for sorting
                            kind        ; debug info
                            bindings)   ; sym -> scope-set -> binding
                     ;; Custom printer:
                     #:property prop:custom-write
                     (lambda (sc port mode)
                       (write-string "#<scope:" port)
                       (display (scope-id sc) port)
                       (write-string ">" port)))

;; A "multi-scope" represents a group of scopes, each of which exists
;; only at a specific phase, and each in a distinct phase. This
;; infinite group of scopes is realized on demand. A multi-scope is
;; used to represent the inside of a module, where bindings in
;; different phases are distinguished by the different scopes within
;; the module's multi-scope.
;;
;; To compute a syntax's set of scopes at a given phase, the
;; phase-specific representative of the multi scope is combined with
;; the phase-independent scopes. Since a multi-scope corresponds to
;; a module, the number of multi-scopes in a syntax is expected to
;; be small.
(serializable-struct multi-scope (id        ; identity
                                  scopes)) ; phase -> representative-scope

(serializable-struct representative-scope scope (owner   ; a multi-scope for which this one is a phase-specific identity
                                    phase)  ; phase of this scope
                     #:property prop:custom-write
                     (lambda (sc port mode)
                       (write-string "#<scope:" port)
                       (display (scope-id sc) port)
                       (when (representative-scope-owner sc)
                         (write-string "=" port)
                         (display (multi-scope-id (representative-scope-owner sc)) port))
                       (write-string "@" port)
                       (display (representative-scope-phase sc) port)
                       (write-string ">" port)))

(serializable-struct shifted-multi-scope (phase        ; phase shift applies to all scopes in multi-scope
                                          multi-scope) ; a multi-scope
                     #:transparent
                     #:property prop:custom-write
                     (lambda (sc port mode)
                       (write-string "#<scope:" port)
                       (display (multi-scope-id (shifted-multi-scope-multi-scope sc)) port)
                       (write-string "@" port)
                       (display (shifted-multi-scope-phase sc) port)
                       (write-string ">" port)))

;; Each new scope increments the counter, so we can check whether one
;; scope is newer than another.
(define id-counter 0)
(define (new-scope-id!)
  (set! id-counter (add1 id-counter))
  id-counter)
  
(define (make-bindings)
  (make-hasheq))

(define (new-scope kind)
  (scope (new-scope-id!) kind (make-bindings)))

(define (new-multi-scope [name (gensym)])
  (shifted-multi-scope 0 (multi-scope (new-scope-id!) (make-hasheqv))))

(define (multi-scope-to-scope-at-phase ms phase)
  ;; Get the identity of `ms` at phase`
  (or (hash-ref (multi-scope-scopes ms) phase #f)
      (let ([s (representative-scope (new-scope-id!) 'module (make-bindings) ms phase)])
        (hash-set! (multi-scope-scopes ms) phase s)
        s)))

(define (scope>? sc1 sc2)
  ((scope-id sc1) . > . (scope-id sc2)))

;; FIXME: Adding, removing, or flipping a scope currently recurs
;; through a syntax object eagerly, but it should be lazy
(define (apply-scope s sc op)
  (syntax-map s
              (lambda (tail? x) x)
              (lambda (s d)
                (struct-copy syntax s
                             [e d]
                             [scopes
                              (if (shifted-multi-scope? sc)
                                  (syntax-scopes s)
                                  (op (syntax-scopes s) sc))]
                             [shifted-multi-scopes
                              (if (shifted-multi-scope? sc)
                                  (op (syntax-shifted-multi-scopes s) sc)
                                  (syntax-shifted-multi-scopes s))]))))

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

(define (add-scopes s scs)
  (for/fold ([s s]) ([sc (in-list scs)])
    (add-scope s sc)))

(define (remove-scope s sc)
  (apply-scope s (generalize-scope sc) set-remove))

(define (remove-scopes s scs)
  (for/fold ([s s]) ([sc (in-list scs)])
    (remove-scope s sc)))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (apply-scope s (generalize-scope sc) set-flip))

(define (flip-scopes s scs)
  (for/fold ([s s]) ([sc (in-list scs)])
    (flip-scope s sc)))

;; To shift a syntax's phase, we only have to shift the phase
;; of any phase-specific scopes. The bindings attached to a
;; scope must be represented in such a s way that the binding
;; shift is implicit via the phase in which the binding
;; is resolved.
(define (shift-multi-scope sms delta)
  (if (zero? delta)
      sms
      (shifted-multi-scope (phase+ delta (shifted-multi-scope-phase sms))
                           (shifted-multi-scope-multi-scope sms))))

;; FIXME: this should be lazy, too?
(define (syntax-shift-phase-level s phase)
  (if (eqv? phase 0)
      s
      (syntax-map s
                  (lambda (tail? d) d)
                  (lambda (s d)
                    (struct-copy syntax s
                                 [e d]
                                 [shifted-multi-scopes
                                  (for/set ([sms (in-set (syntax-shifted-multi-scopes s))])
                                    (shift-multi-scope sms phase))])))))

;; ----------------------------------------

;; Assemble the complete set of scopes at a given phase by extracting
;; a phase-specific representative from each multi-scope.
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
  (and (eq? (syntax-e a)
            (syntax-e b))
       (equal? (syntax-scope-set a phase)
               (syntax-scope-set b phase))))
