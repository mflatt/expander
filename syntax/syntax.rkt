#lang racket/base
(require "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/set.rkt"
         "../common/inline.rkt"
         "preserved.rkt"
         "tamper.rkt"
         "datum-map.rkt")

(provide
 (struct-out syntax) ; includes `syntax?`
 empty-syntax
 identifier?
 
 syntax->datum
 datum->syntax
 
 syntax-map
 non-syntax-map
 
 prop:propagation
 
 deserialize-syntax
 current-arm-inspectors)

(struct syntax ([content #:mutable] ; datum and nested syntax objects; mutated for lazy propagation
                scopes  ; scopes that apply at all phases
                shifted-multi-scopes ; scopes with a distinct identity at each phase; maybe a fallback search
                [scope-propagations #:mutable] ; lazy propogation info
                mpi-shifts ; chain of module-path-index substitutions
                srcloc  ; source location
                props   ; properties
                inspector ; inspector for access to protected bindings
                [tamper #:mutable]) ; see "tamper.rkt"
        ;; Custom printer:
        #:property prop:custom-write
        (lambda (s port mode)
          (write-string "#<syntax" port)
          (define srcloc (syntax-srcloc s))
          (when srcloc
            (define srcloc-str (srcloc->string srcloc))
            (when srcloc-str
              (fprintf port ":~a" srcloc-str)))
          (fprintf port " ~.s" (syntax->datum s))
          (write-string ">" port))
        #:property prop:serialize
        (lambda (s ser state)
          (define prop (syntax-scope-propagations s))
          (define content
            (if prop
                ((propagation-ref prop) s)
                (syntax-content s)))
          (define properties
            (intern-properties
             (syntax-props s)
             (lambda ()
               (for/hasheq ([(k v) (in-hash (syntax-props s))]
                            #:when (preserved-property-value? v))
                 (values k (check-value-to-preserve (plain-property-value v) syntax?))))
             state))
          (define tamper
            (serialize-tamper (syntax-tamper s)))
          (define context-triple
            (intern-context-triple (intern-scopes (syntax-scopes s) state)
                                   (intern-shifted-multi-scopes (syntax-shifted-multi-scopes s) state)
                                   (intern-mpi-shifts (syntax-mpi-shifts s) state)
                                   state))
          ;; Value that is interpreted for deserialization --- based
          ;; on the length of the vector, so make sure changes don't
          ;; create an ambiguity!
          (cond
           [(or properties tamper)
            `#(,(ser content)
               ,(ser context-triple #t)
               ,(ser (syntax-srcloc s) #t)
               ,(ser properties)
               ,(ser tamper))]
           [else
            `#(,(ser content)
               ,(ser context-triple #t)
               ,(ser (syntax-srcloc s) #t))]))
        #:property prop:reach-scopes
        (lambda (s reach)
          (define prop (syntax-scope-propagations s))
          (reach (if prop
                     ((propagation-ref prop) s)
                     (syntax-content s)))
          (reach (syntax-scopes s))
          (reach (syntax-shifted-multi-scopes s))
          (for ([(k v) (in-immutable-hash (syntax-props s))]
                #:when (preserved-property-value? (plain-property-value v)))
            (reach v))))

;; Property to abstract over handling of propagation for
;; serialization; property value takes a syntax object and
;; returns its content
(define-values (prop:propagation propagation? propagation-ref)
  (make-struct-type-property 'propagation))

;; ----------------------------------------

(define empty-scopes (seteq))
(define empty-shifted-multi-scopes (seteq))
(define empty-mpi-shifts null)
(define empty-props #hasheq())

(define empty-syntax
  (syntax #f
          empty-scopes
          empty-shifted-multi-scopes
          #f   ; scope-propogations
          empty-mpi-shifts
          #f   ; srcloc
          empty-props
          #f   ; inspector
          #f)) ; tamper (clean)

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-content s))))

(define (syntax->datum s)
  (syntax-map s (lambda (tail? x) x) (lambda (s d) d) syntax-content))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f])
  (define (wrap content)
    (syntax content
            (if stx-c
                (syntax-scopes stx-c)
                empty-scopes)
            (if stx-c
                (syntax-shifted-multi-scopes stx-c)
                empty-shifted-multi-scopes)
            #f
            (if stx-c
                (syntax-mpi-shifts stx-c)
                empty-mpi-shifts)
            (and stx-l (syntax-srcloc stx-l))
            (if stx-p (syntax-props stx-p) empty-props)
            (and stx-c
                 (syntax-inspector stx-c))
            (and stx-c
                 (syntax-tamper stx-c)
                 (tamper-tainted-for-content content))))
  (non-syntax-map s
                  (lambda (tail? x) (if tail? x (wrap x)))
                  (lambda (s) s)
                  disallow-cycles))

;; `(syntax-map s f d->s)` walks over `s`:
;; 
;;  * `(f tail? d)` is called to each datum `d`, where `tail?`
;;    indicates that the value is a pair/null in a `cdr` --- so that it
;;    doesn't need to be wrapped for `datum->syntax`, for example
;;
;;  * `(d->s orig-s d)` is called for each syntax object,
;;    and the second argument is result of traversing its datum
;; 
;;  * the `s-e` function extracts content of a syntax object
;;
;; The optional `seen` argument is an `eq?`-based immutable hash table
;; to detect and reject cycles. See `datum-map`.

(define-inline (syntax-map s f d->s s-e [seen #f])
  (let loop ([s s])
    (datum-map s
               (lambda (tail? v)
                 (cond
                  [(syntax? v) (d->s v (loop (s-e v)))]
                  [else (f tail? v)]))
               seen)))

;; `(non-syntax-map s f s->)` is like `(syntax-map s f d->s)`, except that
;;  when a syntax object is found, it is just passed to `d` --- so there's
;;  no `d->s` or `s-e`, since they would not be called

(define-inline (non-syntax-map s f [s-> (lambda (x) x)] [seen #f])
  (let loop ([s s])
    (datum-map s
               (lambda (tail? v)
                 (cond
                  [(syntax? v) (s-> v)]
                  [else (f tail? v)]))
               seen)))

(define disallow-cycles
  (hasheq 'cycle-fail
          (lambda (s)
            (raise-arguments-error 'datum->syntax
                                   "cannot create syntax from cyclic datum"
                                   s))))

;; ----------------------------------------

;; Called by the deserializer
(define (deserialize-syntax content context-triple srcloc props tamper inspector)
  (syntax content
          (vector-ref context-triple 0)
          (vector-ref context-triple 1)
          #f
          (vector-ref context-triple 2)
          srcloc
          (if props
              (for/hasheq ([(k v) (in-immutable-hash props)])
                (values k (preserved-property-value v)))
              empty-props)
          inspector
          (deserialize-tamper tamper)))
