#lang racket/base
(require racket/set
         racket/serialize
         "memo.rkt"
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

         add-binding!
         add-bulk-binding!
         
         prop:bulk-binding
         (struct-out bulk-binding-class)

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
                            bindings    ; [mutable] #f or sym -> scope-set -> binding [shadows any bulk binding]
                            bulk-bindings) ; [mutable] list of bulk-binding-at [earlier shadows layer]
                     #:mutable ; for serialization, since some fields are mutable
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
                     #:mutable ; for serialization, since parent has mutable fields
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

(serializable-struct bulk-binding-at (scopes ; scope set
                                      bulk)) ; bulk-binding

;; Bulk bindings are represented by a property, so that the implemenation
;; can be separate and manage serialiation:
(define-values (prop:bulk-binding bulk-binding? bulk-binding-ref)
  (make-struct-type-property 'bulk-binding))

;; Value of `prop:bulk-binding`
(struct bulk-binding-class (get-symbols ; bulk-binding -> sym -> binding-info
                            create))    ; bul-binding -> binding-info sym -> binding
(define (bulk-binding-symbols b)
  ((bulk-binding-class-get-symbols (bulk-binding-ref b)) b))
(define (bulk-binding-create b)
  (bulk-binding-class-create (bulk-binding-ref b)))

;; Each new scope increments the counter, so we can check whether one
;; scope is newer than another.
(define id-counter 0)
(define (new-scope-id!)
  (set! id-counter (add1 id-counter))
  id-counter)
  
(define (make-bindings)
  #f)

(define (make-bulk-bindings)
  null)

(define (new-scope kind)
  (scope (new-scope-id!) kind (make-bindings) (make-bulk-bindings)))

(define (new-multi-scope [name (gensym)])
  (shifted-multi-scope 0 (multi-scope (new-scope-id!) (make-hasheqv))))

(define (multi-scope-to-scope-at-phase ms phase)
  ;; Get the identity of `ms` at phase`
  (or (hash-ref (multi-scope-scopes ms) phase #f)
      (let ([s (representative-scope (new-scope-id!) 'module
                                     (make-bindings) (make-bulk-bindings)
                                     ms phase)])
        (hash-set! (multi-scope-scopes ms) phase s)
        s)))

(define (scope>? sc1 sc2)
  ((scope-id sc1) . > . (scope-id sc2)))

;; FIXME: Adding, removing, or flipping a scope currently recurs
;; through a syntax object eagerly, but it should be lazy
(define (apply-scope s sc op)
  (define-memo-lite (do-op scs)
    (op scs sc))
  (syntax-map s
              (lambda (tail? x) x)
              (lambda (s d)
                (struct-copy syntax s
                             [e d]
                             [scopes
                              (if (shifted-multi-scope? sc)
                                  (syntax-scopes s)
                                  (do-op (syntax-scopes s)))]
                             [shifted-multi-scopes
                              (if (shifted-multi-scope? sc)
                                  (do-op (syntax-shifted-multi-scopes s))
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
      (let ()
        (define-memo-lite (shift-all smss)
          (for/set ([sms (in-set smss)])
            (shift-multi-scope sms phase)))
        (syntax-map s
                    (lambda (tail? d) d)
                    (lambda (s d)
                      (struct-copy syntax s
                                   [e d]
                                   [shifted-multi-scopes
                                    (shift-all (syntax-shifted-multi-scopes s))]))))))

;; ----------------------------------------

;; Assemble the complete set of scopes at a given phase by extracting
;; a phase-specific representative from each multi-scope.
(define (syntax-scope-set s phase)
  (for/fold ([scopes (syntax-scopes s)]) ([sms (in-set (syntax-shifted-multi-scopes s))])
    (set-add scopes (multi-scope-to-scope-at-phase (shifted-multi-scope-multi-scope sms)
                                                   (phase- (shifted-multi-scope-phase sms)
                                                           phase)))))

(define (find-max-scope scopes)
  (when (set-empty? scopes)
    (error "cannot bind in empty scope set"))
  (for/fold ([max-sc (set-first scopes)]) ([sc (in-set scopes)])
    (if (scope>? sc max-sc)
        sc
        max-sc)))

(define (add-binding-in-scopes! scopes sym binding)
  (define max-sc (find-max-scope scopes))
  (define bindings (or (scope-bindings max-sc)
                       (let ([h (make-hasheq)])
                         (set-scope-bindings! max-sc h)
                         h)))
  (define sym-bindings (or (hash-ref bindings sym #f)
                           (let ([h (make-hash)])
                             (hash-set! bindings sym h)
                             h)))
  (hash-set! sym-bindings scopes binding))

(define (add-binding! id binding phase)
  (add-binding-in-scopes! (syntax-scope-set id phase) (syntax-e id) binding))

(define (add-bulk-binding! s binding phase)
  (define scopes (syntax-scope-set s phase))
  (define max-sc (find-max-scope scopes))
  (set-scope-bulk-bindings! max-sc
                            (cons (bulk-binding-at scopes binding)
                                  (scope-bulk-bindings max-sc)))
  (remove-matching-bindings! (scope-bindings max-sc) scopes binding))


;; ----------------------------------------

(define (resolve s phase #:exactly? [exactly? #f])
  (unless (identifier? s)
    (raise-argument-error 'resolve "identifier?" s))
  (unless (phase? phase)
    (raise-argument-error 'resolve "phase?" phase))
  (define scopes (syntax-scope-set s phase))
  (define sym (syntax-e s))
  (define candidates
    (for*/list ([sc (in-set scopes)]
                [bindings (in-value (or (hash-ref (or (scope-bindings sc) #hasheq()) sym #f)
                                        ;; Check bulk bindings; if a symbol match is found,
                                        ;; synthesize a non-bulk binding table
                                        (for/or ([bulk-at (in-list (scope-bulk-bindings sc))])
                                          (define bulk (bulk-binding-at-bulk bulk-at))
                                          (define b-info (hash-ref (bulk-binding-symbols bulk) sym #f))
                                          (and b-info
                                               (hasheq (bulk-binding-at-scopes bulk-at)
                                                       ((bulk-binding-create bulk) bulk b-info sym))))))]
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

;; The bindings of `bulk at `scopes` should shadow any existing
;; mappings in `sym-bindings`
(define (remove-matching-bindings! bindings scopes bulk)
  (define bulk-symbols (bulk-binding-symbols bulk))
  (cond
   [(not bindings) (void)]
   [((hash-count bindings) . < . (hash-count bulk-symbols))
    ;; Faster to consider each sym in `sym-binding`
    (for ([(sym sym-bindings) (in-hash bindings)])
      (when (hash-ref bulk-symbols sym #f)
        (hash-remove! sym-bindings scopes)))]
   [else
    ;; Faster to consider each sym in `bulk-symbols`
    (for ([sym (in-hash-keys bulk-symbols)])
      (define sym-bindings (hash-ref bindings sym #f))
      (when sym-bindings
        (hash-remove! sym-bindings scopes)))]))

;; ----------------------------------------

(define (bound-identifier=? a b phase)
  (and (eq? (syntax-e a)
            (syntax-e b))
       (equal? (syntax-scope-set a phase)
               (syntax-scope-set b phase))))
