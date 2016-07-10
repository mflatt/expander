#lang racket/base
(require (for-syntax racket/base)
         "serialize-property.rkt"
         "serialize-state.rkt"
         "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/module-binding.rkt"
         "../syntax/local-binding.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/module-path.rkt"
         "module-use.rkt"
         "../host/linklet.rkt"
         "built-in-symbol.rkt"
         "reserved-symbol.rkt")

;; Serializaiton is mostly for syntax object and module path indexes.
;;
;; Serialization is implemented by a combination of direct handling
;; for some primitive datatypes, `prop:serialize` handlers attached
;; to some structure types, and deserialization functions provided
;; by the same modules as the serialization handlers.
;;
;; Module path indexes are serialized to code that runs to reconstruct
;; the module path indexes. Syntax objects and other data is
;; serialized to somewhat expression-shaped data and interpreted for
;; deserialization, where that interpretation can refer to an array of
;; already-deserialized module path indexes.
;;
;; To support sharing and cycles, serialized data is represented by:
;;
;;  - a vector of "shell" descriptions to allocate mutatable objects,
;;    such as mutable vectors and hash tables;
;;
;;  - a vector of initializations for shared, immutable values (which
;;    can refer to mutable values)
;;
;;  - a vector of "fill" descriptions to complete the construction of
;;    mutable values (whcih can refer to mutable and shared values);
;;    and
;;
;;  - a final value construction (which can refer to shared and
;;    mutable values).
;;
;; In general, a deserialized object is represented as a pair of a
;; symbol tag and data, including a `quote` tag to represent arbitrary
;; quoted data (that's non-cyclic and with no internal sharing). A few
;; special cases enable a more compact representation:
;;
;;  - numbers, booleans, and symbols are represented as themselves
;;    (i.e., self-quoting, in a sense);
;;
;;  - #&<number> is a reference to a mutable or shared value at
;;    position <number> in a deserialization array;
;;
;;  - #(<elem> ...) is a `srcloc`
;;
;;  - #:inspector and #:bulk-binding-registry refer to
;;    instantiation-time values supplied as imported to the
;;    deserializing linklet
;;
;; In addition to all the complexities of detecting sharing and cycles
;; and breaking cycles on mutable boundaries, the serialization
;; process also prunes unreachable scopes and interns some values that
;; formerly were not shared.

(provide make-module-path-index-table
         add-module-path-index!
         generate-module-path-index-deserialize
         mpis-as-vector
         mpi-vector-id
         
         generate-deserialize

         deserialize-instance
         deserialize-imports
         
         serialize-module-uses)

;; ----------------------------------------
;; Module path index serialization

(define mpi-vector-id (make-built-in-symbol! 'mpi-vector))

(define (make-module-path-index-table)
  (make-hasheq)) ; module path index -> pos

(define (add-module-path-index! mpis mpi)
  (define pos
    (add-module-path-index!/pos mpis mpi))
  (and pos
       `(vector-ref ,mpi-vector-id ,pos)))

(define (add-module-path-index!/pos mpis mpi)
  (cond
   [(not mpi) #f]
   [mpi
    (or (hash-ref mpis mpi #f)
        (let ([pos (hash-count mpis)])
          (hash-set! mpis mpi pos)
          pos))]))

(define (generate-module-path-index-deserialize mpis)
  (define gen-order (make-hasheqv))
  (define rev-mpis
    (for/hasheqv ([(k v) (in-hash mpis)])
      (values v k)))
  ;; Create mpis used earlier first:
  (for ([i (in-range (hash-count rev-mpis))])
    (define mpi (hash-ref rev-mpis i))
    (let loop ([mpi mpi])
      (unless (hash-ref gen-order mpi #f)
        (define-values (name base) (module-path-index-split mpi))
        (when base
          (loop base))
        (hash-set! gen-order mpi (hash-count gen-order)))))
  (define (mpi-id i)
    (string->symbol (format "mpi~a" i)))
  (define rev-gen-order
    (for/hasheqv ([(k v) (in-hash gen-order)])
      (values v k)))
  (define gens
    (for/list ([i (in-range (hash-count gen-order))])
      (define mpi (hash-ref rev-gen-order i))
      (define-values (path base) (module-path-index-split mpi))
      `[(,(mpi-id i))
        ,(cond
          [(top-level-module-path-index? mpi)
           `(deserialize-module-path-index)]
          [(not path)
           `(deserialize-module-path-index ',(or (resolved-module-path-name
                                                  (module-path-index-resolved mpi))
                                                 'self))]
          [else
           `(deserialize-module-path-index ',path ,(and base
                                                        (mpi-id (hash-ref gen-order base))))])]))
  (make-let*
   gens
   `(vector ,@(for/list ([i (in-range (hash-count rev-mpis))])
                (mpi-id (hash-ref gen-order (hash-ref rev-mpis i)))))))

(define (mpis-as-vector mpis)
  (define vec (make-vector (hash-count mpis) #f))
  (for ([(mpi pos) (in-hash mpis)])
    (vector-set! vec pos mpi))
  vec)

;; Convert `let*` into chunks of `let` as much as possible
(define (make-let* bindings body)
  (let loop ([vars #hasheq()] [group null] [bindings bindings])
    (cond
     [(null? bindings) `(let-values ,(reverse group) ,body)]
     [(has-symbol? (cadar bindings) vars)
      `(let-values ,(reverse group) ,(loop #hasheq() null bindings))]
     [else
      (loop (hash-set vars (caaar bindings) #t)
            (cons (car bindings) group)
            (cdr bindings))])))

(define (has-symbol? d vars)
  (or (and (symbol? d) (hash-ref vars d #f))
      (and (pair? d)
           (or (has-symbol? (car d) vars)
               (has-symbol? (cdr d) vars)))))

;; ----------------------------------------
;; Module-use serialization --- as an expression, like module path
;; indexes, and unlike everything else

(define (serialize-module-uses mus mpis)
  (for/list ([mu (in-list mus)])
    `(module-use
      ,(add-module-path-index! mpis (module-use-module mu))
      ,(module-use-phase mu))))

(define (interned-literal? v)
  (or (null? v)
      (boolean? v)
      (and (fixnum? v)
           (v . < . (sub1 (expt 2 30)))
           (v . > . (- (expt 2 30))))
      (and (symbol? v)
           (symbol-interned? v))
      (char? v)
      (keyword? v)))

;; ----------------------------------------
;; Serialization for everything else

(define (generate-deserialize v mpis #:syntax-support? [syntax-support? #t])
  (define reachable-scopes (find-reachable-scopes v))
  
  (define state (make-serialize-state reachable-scopes))
  
  (define mutables (make-hasheq)) ; v -> pos
  (define objs (make-hasheq))     ; v -> step
  (define shares (make-hasheq))   ; v -> #t
  (define obj-step 0)
  
  ;; Build table of sharing and mutable values
  (define frontier null)
  (define (add-frontier! v) (set! frontier (cons v frontier)))
  (let frontier-loop ([v v])
    (let loop ([v v])
      (cond
       [(or (interned-literal? v)
            (module-path-index? v))
        ;; no need to find sharing
        (void)]
       [(hash-ref objs v #f)
        (unless (hash-ref mutables v #f)
          (hash-set! shares v #t))]
       [else
        (cond
         [(serialize-fill!? v)
          ;; Assume no sharing in non-mutable part
          (hash-set! mutables v (hash-count mutables))
          ((serialize-fill!-ref v) v add-frontier! state)]
         [(serialize? v)
          ((serialize-ref v) v loop state)]
         [(pair? v)
          (loop (car v))
          (loop (cdr v))]
         [(vector? v)
          (if (or (immutable? v)
                  (zero? (vector-length v)))
              (for ([e (in-vector v)])
                (loop e))
              (begin
                (hash-set! mutables v (hash-count mutables))
                (for ([e (in-vector v)])
                  (add-frontier! e))))]
         [(box? v)
          (if (immutable? v)
              (loop (unbox v))
              (begin
                (hash-set! mutables v (hash-count mutables))
                (add-frontier! (unbox v))))]
         [(hash? v)
          (if (immutable? v)
              (for ([(k v) (in-hash v)])
                (loop k)
                (loop v))
              (begin
                (hash-set! mutables v (hash-count mutables))
                (for ([(k v) (in-hash v)])
                  (add-frontier! k)
                  (add-frontier! v))))]
         [(prefab-struct-key v)
          (for ([e (in-vector (struct->vector v) 1)])
            (loop e))]
         [(srcloc? v)
          (for ([e (in-vector (struct->vector v) 1)])
            (loop e))]
         [else
          (void)])
        ;; `v` may already be in `objs`, but to get the order right
        ;; for unmarshaling, we need to map it to ka new step number
        (hash-set! objs v obj-step)
        (set! obj-step (add1 obj-step))]))
    (unless (null? frontier)
      (define l frontier)
      (set! frontier null)
      (for ([v (in-list l)])
        (frontier-loop v))))

  ;; Maybe object steps to positions in a vector after mutables
  (define num-mutables (hash-count mutables))
  (define share-step-positions
    (let ([share-steps (for/list ([obj (in-hash-keys shares)])
                         (hash-ref objs obj))])
      (for/hasheqv ([step (in-list (sort share-steps <))]
                    [pos (in-naturals num-mutables)])
        (values step pos))))

  ;; Handle a reference to an object that may be shared
  ;; or mutable
  (define (ser v)
    (cond
     [(hash-ref shares v #f)
      (box-immutable (hash-ref share-step-positions (hash-ref objs v)))]
     [(hash-ref mutables v #f)
      => (lambda (n) (box-immutable n))]
     [else
      (do-ser v)]))
  
  ;; Handle an immutable, not-shared (or on RHS of binding) value
  (define (do-ser v)
    (cond
     [(module-path-index? v)
      `(mpi . ,(add-module-path-index!/pos mpis v))]
     [(serialize? v)
      ((serialize-ref v) v ser state)]
     [(pair? v)
      (define a (ser (car v)))
      (define d (ser (cdr v)))
      (cond
       [(and (quoted? a) (quoted? d))
        (quoted v)]
       [(and (pair? d)
             (eq? 'list (car d)))
        (list* 'list a (cdr d))]
       [(and (pair? d)
             (eq? 'quote (car d))
             (eq? '() (cdr d)))
        `(list ,a)]
       [else
        `(cons ,a ,d)])]
     [(null? v) (quoted '())]
     [(box? v)
      (define content (ser (unbox v)))
      (if (quoted? content)
          (quoted v)
          `(box-immutable . ,content))]
     [(vector? v)
      (define content (for/list ([e (in-vector v)])
                        (ser e)))
      (if (andmap quoted? content)
          (quoted v)
          (cons 'vector-immutable (list->vector content)))]
     [(hash? v)
      (define k-content (for/list ([k (in-hash-keys v)])
                          (ser k)))
      (define v-content (for/list ([v (in-hash-values v)])
                          (ser v)))
      (if (and (andmap quoted? k-content)
               (andmap quoted? v-content))
          (quoted v)
          `(,(cond
              [(hash-eq? v) 'hasheq]
              [(hash-eqv? v) 'hasheqv]
              [else 'hash])
            ,(list->vector k-content)
            ,(list->vector v-content)))]
     [(prefab-struct-key v)
      => (lambda (k)
           (define content
             (for/list ([e (in-vector (struct->vector v) 1)])
               (ser e)))
           (if (andmap quoted? content)
               (quoted v)
               `(make-prefab-struct ,k ,@content)))]
     [(srcloc? v)
      ;; srcloc is encoded as a plain vector
      (for/vector ([e (in-vector (struct->vector v) 1)])
        (ser e))]
     [else (quoted v)]))
  
  ;; Generate the shell of a mutable value --- uses a different
  ;; encoding then the one for most other purposes
  (define (ser-shell v)
    (cond
     [(serialize-fill!? v) ((serialize-ref v) v ser state)]
     [(box? v) 'box]
     [(vector? v) (vector-length v)]
     [(hash? v) (cond
                 [(hash-eq? v) 'hasheq]
                 [(hash-eqv? v) 'hasheqv]
                 [else 'hash])]
     [else
      (error 'ser-shell "unknown mutable: ~e" v)]))
  
  ;; Fill in the content of a mutable shell --- also a different
  ;; encoding
  (define (ser-shell-fill v)
    (cond
     [(serialize-fill!? v) ((serialize-fill!-ref v) v ser state)]
     [(box? v) `(set-box! ,(ser (unbox v)))]
     [(vector? v) `(set-vector!
                    ,(list->vector
                      (for/list ([v (in-vector v)]) (ser v))))]
     [(hash? v) `(hash-set!
                  ,(list->vector
                    (for/list ([k (in-hash-keys v)]) (ser k)))
                  ,(list->vector
                    (for/list ([v (in-hash-values v)]) (ser v))))]
     [else
      (error 'ser-shell-fill "unknown mutable: ~e" v)]))
  
  ;; Prepare mutable shells, first:
  (define rev-mutables (for/hasheqv ([(k v) (in-hash mutables)])
                         (values v k)))
  (define mutable-shell-bindings
    (for/vector #:length (hash-count mutables) ([i (in-range (hash-count mutables))])
                (ser-shell (hash-ref rev-mutables i))))
  
  ;; Prepare shared values:
  (define rev-shares (for/hasheqv ([obj (in-hash-keys shares)])
                       (values (hash-ref share-step-positions (hash-ref objs obj))
                               obj)))
  (define shared-bindings
    (for/vector #:length (hash-count shares) ([i (in-range num-mutables (+ num-mutables (hash-count shares)))])
                (do-ser (hash-ref rev-shares i))))
  
  ;; Fill in mutable values
  (define mutable-fills
    (for/vector #:length (hash-count mutables) ([i (in-range (hash-count mutables))])
                (ser-shell-fill (hash-ref rev-mutables i))))
  
  ;; Put it all together:
  `(deserialize
    ,mpi-vector-id
    ,(if syntax-support? inspector-id #f)
    ,(if syntax-support? bulk-binding-registry-id #f)
    ',mutable-shell-bindings
    ',shared-bindings
    ',mutable-fills
    ',(ser v)))

  
(define (quoted? v)
  (or (number? v)
      (boolean? v)
      (symbol? v)
      (and (pair? v) (eq? 'quote (car v)))))

(define (quoted v)
  (if (or (number? v) (boolean? v) (symbol? v))
      v
      `(quote . ,v)))

;; ----------------------------------------
;; Deserialization

;; The `deserialize` function interprets the encoding generated by
;; serialization (with hardwired calls to various deserialization
;; functions provided by other modules)

(define (deserialize mpis inspector bulk-binding-registry mutable-vec shared-vec mutable-fill-vec result)
  (define num-mutables  (vector-length mutable-vec))
  (define shared (make-vector (+ num-mutables (vector-length shared-vec))
                              'uninit))
  ;; Make mutable shells
  (for ([d (in-vector mutable-vec)]
        [i (in-naturals)])
    (vector-set! shared i (decode-shell d mpis inspector bulk-binding-registry shared)))
  ;; Construct shared values; note that later constructions can refer
  ;; to earlier ones
  (for ([d (in-vector shared-vec)]
        [i (in-naturals num-mutables)])
    (vector-set! shared i (decode d mpis inspector bulk-binding-registry shared)))
  ;; Fill in mutable shells
  (for ([d (in-vector mutable-fill-vec)]
        [v (in-vector shared)]
        [i (in-naturals)])
    (decode-fill! v d mpis inspector bulk-binding-registry shared))
  ;; Construct the final result
  (decode result mpis inspector bulk-binding-registry shared))

;; Decode the construction of a mutable variable
(define (decode-shell d mpis inspector bulk-binding-registry shared)
  (case d
    [(box) (box #f)]
    [(hash) (make-hasheq)]
    [(hasheq) (make-hasheq)]
    [(hasheqv) (make-hasheqv)]
    [else
     (cond
      [(exact-integer? d) (make-vector d)]
      [else (decode d mpis inspector bulk-binding-registry shared)])]))

;; The decoder that is used for most purposes
(define (decode d mpis inspector bulk-binding-registry shared)
  (let decode ([d d])
    (define-syntax (decode* stx)
      (syntax-case stx ()
        [(_ constr field ...)
         (with-syntax ([(pos ...) (for/list ([f (in-list (syntax->list #'(field ...)))]
                                             [i (in-naturals 1)])
                                    i)])
           #`(constr (decode (list-ref d pos)) ...))]))
    (cond
      [(eq? d '#:inspector) inspector]
      [(eq? d '#:bulk-binding-registry) bulk-binding-registry]
      [(number? d) d]
      [(boolean? d) d]
      [(box? d) (vector-ref shared (unbox d))]
      [(symbol? d) d]
      [(vector? d)
       (srcloc (decode (vector-ref d 0))
               (decode (vector-ref d 1))
               (decode (vector-ref d 2))
               (decode (vector-ref d 3))
               (decode (vector-ref d 4)))]
      [else
       (case (car d)
         [(quote) (cdr d)]
         [(mpi) (vector-ref mpis (cdr d))]
         [(cons) (cons (decode (cadr d))
                       (decode (caddr d)))]
         [(list) (for/list ([d (in-list (cdr d))])
                   (decode d))]
         [(box-immutable)
          (box-immutable (decode (cdr d)))]
         [(vector-immutable)
          (vector->immutable-vector
           (for/vector #:length (vector-length (cdr d)) ([d (in-vector (cdr d))])
                       (decode d)))]
         [(hash)
          (for/hash ([k (in-vector (cadr d))]
                     [v (in-vector (caddr d))])
            (values (decode k) (decode v)))]
         [(hasheq)
          (for/hasheq ([k (in-vector (cadr d))]
                       [v (in-vector (caddr d))])
            (values (decode k) (decode v)))]
         [(hasheqv)
          (for/hasheqv ([k (in-vector (cadr d))]
                        [v (in-vector (caddr d))])
            (values (decode k) (decode v)))]
         [(make-prefab-struct)
          (apply make-prefab-struct
                 (cadr d)
                 (for/list ([d (in-list (cddr d))])
                   (decode d)))]
         [(deserialize-syntax)
          (decode* deserialize-syntax content scopes shifted-multi-scopes mpi-shifts srcloc props inspector tamper)]
         [(deserialize-scope)
          (if (null? (cdr d))
              (deserialize-scope)
              (deserialize-scope (cdr d)))]
         [(deserialize-multi-scope)
          (decode* deserialize-multi-scope name scopes)]
         [(deserialize-shifted-multi-scope)
          (decode* deserialize-shifted-multi-scope phase multi-scope)]
         [(deserialize-bulk-binding-at)
          (decode* deserialize-bulk-binding-at scopes bulk)]
         [(deserialize-representative-scope)
          (decode* deserialize-representative-scope kind phase)]
         [(deserialize-full-module-binding)
          (decode* deserialize-full-module-binding
                   module sym phase
                   nominal-module
                   nominal-phase
                   nominal-sym
                   nominal-require-phase
                   free=id
                   extra-inspector)]
         [(deserialize-simple-module-binding)
          (decode* deserialize-simple-module-binding module sym phase)]
         [(deserialize-full-local-binding)
          (decode* deserialize-full-local-binding key free=id)]
         [(deserialize-bulk-binding)
          (decode* deserialize-bulk-binding mpi provide-phase-level phase-shift bulk-binding-registry)]
         [else
          (error 'deserialize "bad encoding: ~v" d)])])))

;; Decode the filling of mutable values, which has its own encoding
;; variant
(define (decode-fill! v d mpis inspector bulk-binding-registry shared)
  (case (car d)
    [(set-box!) (set-box! v (decode (cadr d) mpis inspector bulk-binding-registry shared))]
    [(set-vector!) (for ([vv (in-vector (cdr d))]
                         [i (in-naturals)])
                     (vector-set! v i (decode vv mpis inspector bulk-binding-registry shared)))]
    [(hash-set!) (for ([hk (in-vector (cadr d))]
                       [hv (in-vector (caddr d))])
                   (hash-set! v
                              (decode hk mpis inspector bulk-binding-registry shared)
                              (decode hv mpis inspector bulk-binding-registry shared)))]
    [(void) (void)]
    [(deserialize-scope-fill!)
     (deserialize-scope-fill! v
                              (decode (cadr d) mpis inspector bulk-binding-registry shared)
                              (decode (caddr d) mpis inspector bulk-binding-registry shared))]
    [(deserialize-representative-scope-fill!)
     (deserialize-representative-scope-fill! v
                                             (decode (cadr d) mpis inspector bulk-binding-registry shared)
                                             (decode (caddr d) mpis inspector bulk-binding-registry shared)
                                             (decode (cadddr d) mpis inspector bulk-binding-registry shared))]
    [else
     (error 'deserialize "bad fill encoding: ~v" d)]))
 
;; ----------------------------------------
;; For pruning unreachable scopes in serialization

(define (find-reachable-scopes v)
  (define seen (make-hasheq))
  (define reachable-scopes (seteq))
  (define scope-triggers (make-hasheq))
  
  (let loop ([v v])
    (cond
     [(interned-literal? v) (void)]
     [(hash-ref seen v #f) (void)]
     [else
      (hash-set! seen v #t)
      (cond
       [(scope-with-bindings? v)
        (set! reachable-scopes (set-add reachable-scopes v))
        
        ((reach-scopes-ref v) v loop)
        
        (define l (hash-ref scope-triggers v null))
        (for ([v (in-list l)])
          (loop v))
 
        ;; A binding may have a `free-id=?` equivalence;
        ;; that equivalence is reachable if all the scopes in the
        ;; binding set are reachable; for a so-far unreachable scope,
        ;; record a trigger in case the scope bcomes reachable later
        ((scope-with-bindings-ref v)
         v
         reachable-scopes
         loop
         (lambda (sc-unreachable b)
           (hash-update! scope-triggers
                         sc-unreachable
                         (lambda (l) (cons b l))
                         null)))]
       [(reach-scopes? v)
        ((reach-scopes-ref v) v loop)]
       [(pair? v)
        (loop (car v))
        (loop (cdr v))]
       [(vector? v)
        (for ([e (in-vector v)])
          (loop e))]
       [(box? v)
        (loop (unbox v))]
       [(hash? v)
        (for ([(k v) (in-hash v)])
          (loop k)
          (loop v))]
       [(prefab-struct-key v)
        (for ([e (in-vector (struct->vector v) 1)])
          (loop e))]
       [else
        (void)])]))
  
  reachable-scopes)

;; ----------------------------------------
;; Set up the instance to import into deserializing linklets

(define deserialize-instance (make-instance 'deserialize))
(define deserialize-imports null)

(define (add! sym val)
  (instance-set-variable-value! deserialize-instance sym val)
  (set! deserialize-imports
        (cons sym deserialize-imports))
  (register-built-in-symbol! sym))
(add! 'deserialize-module-path-index deserialize-module-path-index)
(add! 'syntax-module-path-index-shift syntax-module-path-index-shift)
(add! 'syntax-shift-phase-level syntax-shift-phase-level)
(add! 'module-use module-use)
(add! 'deserialize deserialize)
