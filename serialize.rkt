#lang racket/base
(require "serialize-property.rkt"
         "serialize-state.rkt"
         "set.rkt"
         "syntax.rkt"
         "scope.rkt"
         "binding.rkt"
         "module-binding.rkt"
         "local-binding.rkt"
         "bulk-binding.rkt"
         "module-path.rkt"
         "module-use.rkt"
         "linklet.rkt"
         "built-in-symbol.rkt")

(provide make-module-path-index-table
         add-module-path-index!
         generate-module-path-index-deserialize
         mpis-as-vector
         mpi-vector-id
         
         generate-deserialize

         deserialize-instance
         deserialize-imports
         
         serialize-module-uses)

(define mpi-vector-id (make-built-in-symbol! 'mpi-vector))

(define (make-module-path-index-table)
  (make-hasheq)) ; module path index -> pos

(define (add-module-path-index! mpis mpi)
  (cond
   [(not mpi) #f]
   [mpi
    (define pos
      (or (hash-ref mpis mpi #f)
          (let ([pos (hash-count mpis)])
            (hash-set! mpis mpi pos)
            pos)))
    `(vector-ref ,mpi-vector-id ,pos)]))

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

;; ----------------------------------------
  
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

(define (generate-deserialize v mpis)
  (define reachable-scopes (find-reachable-scopes v))
  
  (define state (make-serialize-state reachable-scopes))
  
  (define mutables (make-hasheq))
  (define objs (make-hasheq))
  (define obj-count 0)
  (define shares (make-hasheq))
  
  (define (shared-id n)
    (string->symbol (format "shared~a" n)))
  (define (mutable-id n)
    (string->symbol (format "mutable~a" n)))
  
  (define (quoted? v)
    (and (pair? v) (eq? 'quote (car v))))
  
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
          ((serialize-fill!-ref v) #f v add-frontier! state)]
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
        (hash-set! objs v obj-count)
        (set! obj-count (add1 obj-count))]))
    (unless (null? frontier)
      (define l frontier)
      (set! frontier null)
      (for ([v (in-list l)])
        (frontier-loop v))))
  
  ;; Handle a reference to an object that may be shared
  ;; or mutable
  (define (ser v)
    (cond
     [(hash-ref shares v #f)
      (shared-id (hash-ref objs v))]
     [(hash-ref mutables v #f)
      => (lambda (n)
           (mutable-id n))]
     [else
      (do-ser v)]))
  
  ;; Handle an immutable, not-shared (or on RHS of binding) value
  (define (do-ser v)
    (cond
     [(module-path-index? v)
      (add-module-path-index! mpis v)]
     [(serialize? v)
      ((serialize-ref v) v ser state)]
     [(pair? v)
      (define a (ser (car v)))
      (define d (ser (cdr v)))
      (cond
       [(and (quoted? a) (quoted? d))
        `(quote ,v)]
       [(and (pair? d)
             (eq? 'list (car d)))
        (list* 'list a (cdr d))]
       [(and (pair? d)
             (eq? 'quote (car d))
             (eq? '() (cadr d)))
        `(list ,a)]
       [else
        `(cons ,a ,d)])]
     [(null? v) '(quote ())]
     [(box? v)
      (define content (ser (unbox v)))
      (if (quoted? content)
          `(quote ,v)
          `(box-immutable ,content))]
     [(vector? v)
      (define content (for/list ([e (in-vector v)])
                        (ser e)))
      (if (andmap quoted? content)
          `(quote ,v)
          (cons 'vector-immutable content))]
     [(hash? v)
      (define k-content (for/list ([k (in-hash-keys v)])
                          (ser k)))
      (define v-content (for/list ([v (in-hash-values v)])
                          (ser v)))
      (if (and (andmap quoted? k-content)
               (andmap quoted? v-content))
          `(quote ,v)
          `(,(cond
              [(hash-eq? v) 'hasheq]
              [(hash-eqv? v) 'hasheqv]
              [else 'hash])
            ,@(let loop ([k-content k-content]
                         [v-content v-content])
                (cond
                 [(null? k-content) null]
                 [else (list* (car k-content)
                              (car v-content)
                              (loop (cdr k-content)
                                    (cdr v-content)))]))))]
     [(prefab-struct-key v)
      => (lambda (k)
           (define content
             (for/list ([e (in-vector (struct->vector v) 1)])
               (ser e)))
           (if (andmap quoted? content)
               `(quote ,v)
               `(make-prefab-struct ',k ,@content)))]
     [(srcloc? v)
      `(srcloc ,@(for/list ([e (in-vector (struct->vector v) 1)])
                   (ser e)))]
     [else `(quote ,v)]))
  
  ;; Generate the shell of a mutable value:
  (define (ser-shell v)
    (cond
     [(serialize-fill!? v) ((serialize-ref v) v ser state)]
     [(box? v) `(box #f)]
     [(vector? v) `(make-vector ,(vector-length v) #f)]
     [(hash? v) (cond
                 [(hash-eq? v) '(make-hasheq)]
                 [(hash-eqv? v) '(make-hasheqv)]
                 [else '(make-hash)])]
     [else
      (error 'ser-shell "unknown mutable: ~e" v)]))
  
  ;; Fill in the content of a mutable shell:
  (define (ser-shell-fill id v)
    (cond
     [(serialize-fill!? v) ((serialize-fill!-ref v) id v ser state)]
     [(box? v) `(set-box! ,id ,(ser (unbox v)))]
     [(vector? v) `(begin
                    ,@(for/list ([v (in-vector v)]
                                 [i (in-range (vector-length v))])
                        `(vector-set! ,id ,i ,(ser v))))]
     [(hash? v) `(begin
                  ,@(for/list ([(k v) (in-hash v)])
                      `(hash-set! ,id ,(ser k) ,(ser v))))]
     [else
      (error 'ser-shell-fill "unknown mutable: ~e" v)]))
  
  ;; Generate mutable shells, first:
  (define rev-mutables (for/hasheqv ([(k v) (in-hash mutables)])
                         (values v k)))
  (define mutable-shell-bindings
    (for/list ([i (in-range (hash-count mutables))])
      `[(,(mutable-id i)) ,(ser-shell (hash-ref rev-mutables i))]))
  
  ;; Generate shared values in reverse order:
  (define rev-shares (for/hasheqv ([v (in-hash-keys shares)])
                       (values (hash-ref objs v) v)))
  (define shared-bindings
    (for/list ([i (in-range obj-count)]
               #:when (hash-ref rev-shares i #f))
      `[(,(shared-id i)) ,(do-ser (hash-ref rev-shares i))]))
  
  ;; Fill in mutable values last
  (define mutable-fills
    (for/list ([i (in-range (hash-count mutables))])
      (ser-shell-fill (mutable-id i) (hash-ref rev-mutables i))))
  
  ;; Put it all together:
  `(let-values ,mutable-shell-bindings
    ,(make-let* 
      shared-bindings
      `(begin
        ,@mutable-fills
        ,(ser v)))))

;; ----------------------------------------

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

(define (serialize-module-uses mus mpis)
  (for/list ([mu (in-list mus)])
    `(module-use
      ,(add-module-path-index! mpis (module-use-module mu))
      ,(module-use-phase mu))))

;; ----------------------------------------

(define deserialize-instance (make-instance 'deserialize))
(define deserialize-imports null)

(define (add! sym val)
  (set-instance-variable-value! deserialize-instance sym val)
  (set! deserialize-imports
        (cons sym deserialize-imports))
  (register-built-in-symbol! sym))
(add! 'deserialize-module-path-index deserialize-module-path-index)
(add! 'deserialize-syntax deserialize-syntax)
(add! 'deserialize-scope deserialize-scope)
(add! 'deserialize-scope-fill! deserialize-scope-fill!)
(add! 'deserialize-multi-scope deserialize-multi-scope)
(add! 'deserialize-shifted-multi-scope deserialize-shifted-multi-scope)
(add! 'deserialize-representative-scope deserialize-representative-scope)
(add! 'deserialize-representative-scope-fill! deserialize-representative-scope-fill!)
(add! 'deserialize-bulk-binding-at deserialize-bulk-binding-at)
(add! 'deserialize-full-module-binding deserialize-full-module-binding)
(add! 'deserialize-simple-module-binding deserialize-simple-module-binding)
(add! 'deserialize-full-local-binding deserialize-full-local-binding)
(add! 'deserialize-bulk-binding deserialize-bulk-binding)
(add! 'syntax-module-path-index-shift syntax-module-path-index-shift)
(add! 'syntax-shift-phase-level syntax-shift-phase-level)
(add! 'module-use module-use)
