#lang racket/base
(require racket/set
         "prefab.rkt")

(provide
 (struct-out syntax) ; includes `syntax?` and `syntax-e`
 empty-syntax
 identifier?
 
 syntax->datum
 datum->syntax
 
 syntax-map
 
 syntax-property)

(struct syntax (e      ; datum and nested syntax objects
                scopes ; scopes that apply at all phases
                shifted-multi-scopes ; scopes with a distinct identity at each phase
                mpi-shifts ; chain of module-path-index substitutions
                srcloc ; source location
                props) ; properties
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
          (write-string ">" port)))

(define empty-scopes (seteq))
(define empty-shifted-multi-scopes (set))
(define empty-mpi-shifts null)
(define empty-props #hash())

(define empty-syntax
  (syntax #f empty-scopes empty-shifted-multi-scopes empty-mpi-shifts #f empty-props))

(define (identifier? s)
  (and (syntax? s) (symbol? (syntax-e s))))

(define (syntax->datum s)
  (syntax-map s (lambda (tail? x) x) (lambda (s d) d)))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f])
  (define (wrap e)
    (syntax e
            (if stx-c
                (syntax-scopes stx-c)
                empty-scopes)
            (if stx-c
                (syntax-shifted-multi-scopes stx-c)
                empty-shifted-multi-scopes)
            (if stx-c
                (syntax-mpi-shifts stx-c)
                empty-mpi-shifts)
            (and stx-l (syntax-srcloc stx-l))
            (if stx-p (syntax-props stx-p) empty-props)))
  (syntax-map s
              (lambda (tail? x) (if tail? x (wrap x)))
              #f))

;; `(syntax-map s f d->s)` walks over `s`:
;; 
;;  * `(f tail? d)` is called to each datum `d`, where `tail?`
;;  indicates that the value is a pair/null in a `cdr` --- so that it
;;  doesn't need to be wrapped for `datum->syntax`, for example
;;
;;  * if `d->s` is #f, then syntax object are returned as-is
;;
;;  * otherwise, `(d->s orig-s d)` is called for each syntax object
;;  and the return of traversing its datum
;;
(define (syntax-map s f d->s)
  (let loop ([tail? #f] [s s])
    (cond
     [(syntax? s) (if d->s
                      (d->s s (loop #f (syntax-e s)))
                      s)]
     [(pair? s) (f tail? (cons (loop #f (car s))
                               (loop #t (cdr s))))]
     [(vector? s) (f #f (vector->immutable-vector
                         (for/vector #:length (vector-length s) ([e (in-vector s)])
                                     (loop #f e))))]
     [(box? s) (f #f (box-immutable (loop #f (unbox s))))]
     [(immutable-prefab-struct-key s)
      => (lambda (key)
           (f #f
              (apply make-prefab-struct
                     key
                     (for/list ([e (in-vector (struct->vector s) 1)])
                       (loop #f e)))))]
     [(and (hash? s) (immutable? s))
      (cond
       [(hash-eq? s)
        (f #f
           (for/hasheq ([(k v) (in-hash s)])
             (values k (loop #f v))))]
       [(hash-eqv? s)
        (f #f
           (for/hasheqv ([(k v) (in-hash s)])
             (values k (loop #f v))))]
       [else
        (f #f
           (for/hash ([(k v) (in-hash s)])
             (values k (loop #f v))))])]
     [(null? s) (f tail? s)]
     [else (f #f s)])))

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
