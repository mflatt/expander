#lang racket/base
(require "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/set.rkt"
         "tamper.rkt"
         "datum-map.rkt")

(provide
 (struct-out syntax) ; includes `syntax?`
 empty-syntax
 identifier?
 
 syntax->datum
 datum->syntax
 
 syntax-map
 
 syntax-property
 syntax-property-symbol-keys
 syntax-property-remove
 
 prop:propagation
 
 deserialize-syntax
 current-arm-inspectors)

(struct syntax ([content #:mutable] ; datum and nested syntax objects; mutated for lazy propagation
                scopes  ; scopes that apply at all phases
                [scope-propagations #:mutable] ; lazy propogation info
                shifted-multi-scopes ; scopes with a distinct identity at each phase; maybe a fallback search
                mpi-shifts ; chain of module-path-index substitutions
                bulk-binding-registry ; for resolving bulk bindings on unmarshal
                srcloc  ; source location
                props   ; properties
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
          `(deserialize-syntax
            ,(ser (if prop
                      ((propagation-ref prop) s)
                      (syntax-content s)))
            ,(ser (intern-scopes (syntax-scopes s) state))
            ,(ser (intern-shifted-multi-scopes (syntax-shifted-multi-scopes s) state))
            ,(ser (intern-mpi-shifts (syntax-mpi-shifts s) state))
            ,(ser (syntax-srcloc s))
            ,(ser (serialize-tamper (syntax-tamper s)))))
        #:property prop:reach-scopes
        (lambda (s reach)
          (define prop (syntax-scope-propagations s))
          (reach (if prop
                     ((propagation-ref prop) s)
                     (syntax-content s)))
          (reach (syntax-scopes s))
          (reach (syntax-shifted-multi-scopes s))))

;; Property to abstract over handling of propagation for
;; serialization; property value takes a syntax object and
;; returns its content
(define-values (prop:propagation propagation? propagation-ref)
  (make-struct-type-property 'propagation))

;; ----------------------------------------

(define empty-scopes (seteq))
(define empty-shifted-multi-scopes (set))
(define empty-mpi-shifts null)
(define empty-props #hash())

(define empty-syntax
  (syntax #f
          empty-scopes
          #f   ; scope-propogations
          empty-shifted-multi-scopes
          empty-mpi-shifts
          #f   ; bulk-binding-registry
          #f   ; srcloc
          empty-props
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
            #f
            (if stx-c
                (syntax-shifted-multi-scopes stx-c)
                empty-shifted-multi-scopes)
            (if stx-c
                (syntax-mpi-shifts stx-c)
                empty-mpi-shifts)
            (and stx-c
                 (syntax-bulk-binding-registry stx-c))
            (and stx-l (syntax-srcloc stx-l))
            (if stx-p (syntax-props stx-p) empty-props)
            (and stx-c
                 (syntax-tamper stx-c)
                 (tamper-tainted-for-content content))))
  (syntax-map s
              (lambda (tail? x) (if tail? x (wrap x)))
              #f
              #f))

;; `(syntax-map s f d->s)` walks over `s`:
;; 
;;  * `(f tail? d)` is called to each datum `d`, where `tail?`
;;    indicates that the value is a pair/null in a `cdr` --- so that it
;;    doesn't need to be wrapped for `datum->syntax`, for example
;;
;;  * if `d->s` is #f, then syntax object are returned as-is
;;
;;  * otherwise, `(d->s orig-s d)` is called for each syntax object,
;;    and the second argument is result of traversing its datum
;; 
;;  * the `s-e` function extrcts content of a syntax object; if it's
;;    #f, then there's no loop over the content
;;
(define (syntax-map s f d->s s-e)
  (let loop ([s s])
    (datum-map s
               (lambda (tail? v)
                 (cond
                  [(syntax? v) (if d->s
                                   (d->s v (if s-e
                                               (loop (s-e v))
                                               (syntax-content v)))
                                   v)]
                  [else (f tail? v)])))))

;; ----------------------------------------

(define syntax-property
  (case-lambda
    [(s key)
     (unless (syntax? s)
       (raise-argument-error 'syntax-property "syntax" s))
     (hash-ref (syntax-props s) key #f)]
    [(s key val)
     (unless (syntax? s)
       (raise-argument-error 'syntax-property "syntax" s))
     (struct-copy syntax s
                  [props (hash-set (syntax-props s) key val)])]))

(define syntax-property-symbol-keys
  (lambda (s)
    (unless (syntax? s)
      (raise-argument-error 'syntax-property-symbol-keys "syntax" s))
    (for/list ([(k v) (in-hash (syntax-props s))]
               #:when (and (symbol? k) (symbol-interned? k)))
      k)))

(define (syntax-property-remove s key)
  (if (hash-ref (syntax-props s) key #f)
      (struct-copy syntax s
                   [props (hash-remove (syntax-props s) key)])
      s))

;; ----------------------------------------

(define (deserialize-syntax content scopes shifted-multi-scopes mpi-shifts srcloc tamper)
  (syntax content
          scopes #f shifted-multi-scopes
          mpi-shifts #f srcloc empty-props
          (deserialize-tamper tamper)))
