#lang racket/base
(require (for-syntax racket/base)
         "../common/set.rkt"
         "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "syntax.rkt")

;; A binding table within a scope maps symbol plus scope set
;; combinations (where the scope binding the binding table is always
;; included in the set).
;;
;; A binding table is one of
;;
;;  - #f
;;
;;  - (single-binding-table sym scope-set binding)
;;
;;  - hash of sym -> (single-binding-value scope-set binding)
;;                    or hash of scope-set -> binding
;;
;;  - hash of sym -> (single-binding-value scope-set binding)
;;
;;  - (table-with-bulk-bindings hash list-of-bulk-binding-at)

(provide empty-binding-table
         binding-table-add
         binding-table-add-bulk
         binding-table-empty?
         
         in-binding-table
         
         prop:bulk-binding
         (struct-out bulk-binding-class)
         
         binding-table-prune-to-reachable
         binding-table-register-reachable
         
         deserialize-single-binding-table
         deserialize-single-binding-value
         deserialize-table-with-bulk-bindings
         deserialize-bulk-binding-at)

(define empty-binding-table #f)

(struct single-binding-table (sym scopes binding)
        #:property prop:serialize
        (lambda (sbt ser state)
          `(deserialize-single-binding-table
            ,(ser (single-binding-table-sym sbt))
            ,(ser (single-binding-table-scopes sbt))
            ,(ser (single-binding-table-binding sbt)))))
          
(struct single-binding-value (scopes binding)
        #:property prop:serialize
        (lambda (sbv ser state)
          `(deserialize-single-binding-value
            ,(ser (single-binding-value-scopes sbv))
            ,(ser (single-binding-value-binding sbv)))))
        
(struct table-with-bulk-bindings (syms bulk-bindings)
        #:property prop:serialize
        (lambda (twbb ser state)
          `(deserialize-table-with-bulk-bindings
            ,(ser (table-with-bulk-bindings-syms twbb))
            ,(ser (table-with-bulk-bindings-bulk-bindings twbb)))))

(define (deserialize-single-binding-table sym scopes binding)
  (single-binding-table sym scopes binding))

(define (deserialize-single-binding-value scopes binding)
  (single-binding-value scopes binding))

(define (deserialize-table-with-bulk-bindings syms bulk-bindings)
  (table-with-bulk-bindings syms bulk-bindings))

;; ----------------------------------------

(struct bulk-binding-at (scopes ; scope set
                         bulk)  ; bulk-binding
        #:property prop:serialize
        (lambda (bba ser state)
          ;; Data that is interpreted by the deserializer:
          `(deserialize-bulk-binding-at
            ,(ser (bulk-binding-at-scopes bba))
            ,(ser (bulk-binding-at-bulk bba))))
        #:property prop:reach-scopes
        (lambda (sms reach)
          ;; bulk bindings are pruned dependong on whether all scopes
          ;; in `scopes` are reachable, and we shouldn't get here
          ;; when looking for scopes
          (error "shouldn't get here")))

(define (deserialize-bulk-binding-at scopes bulk)
  (bulk-binding-at scopes bulk))

;; Bulk bindings are represented by a property, so that the implementation
;; can be separate and manage serialiation:
(define-values (prop:bulk-binding bulk-binding? bulk-binding-ref)
  (make-struct-type-property 'bulk-binding))

;; Value of `prop:bulk-binding`
(struct bulk-binding-class (get-symbols ; bulk-binding list-of-shift -> sym -> binding-info
                            create))    ; bul-binding -> binding-info sym -> binding
(define (bulk-binding-symbols b s extra-shifts)
  ;; Providing the identifier `s` supports its shifts
  ((bulk-binding-class-get-symbols (bulk-binding-ref b))
   b 
   (append extra-shifts (if s (syntax-mpi-shifts s) null))))
(define (bulk-binding-create b)
  (bulk-binding-class-create (bulk-binding-ref b)))

;; ----------------------------------------

(define (binding-table-empty? bt)
  (not bt))

;; Adding a binding for a single symbol
(define (binding-table-add bt scopes sym binding)
  (cond
   [(not bt) (single-binding-table sym scopes binding)]
   [(single-binding-table? bt)
    (define new-bt (single-to-hash bt))
    (binding-table-add new-bt scopes sym binding)]
   [(hash? bt)
    (define v (hash-ref bt sym #f))
    (cond
     [(not v) (hash-set bt sym (single-binding-value scopes binding))]
     [(hash? v) (hash-set bt sym (hash-set v scopes binding))]
     [else (hash-set bt sym (hash (single-binding-value-scopes v)
                                  (single-binding-value-binding v)
                                  ;; After original, just in case overriding
                                  scopes binding))])]
   [else
    (struct-copy table-with-bulk-bindings bt 
                 [syms (binding-table-add (table-with-bulk-bindings-syms bt)
                                          scopes
                                          sym
                                          binding)])]))

(define (single-to-hash bt)
  (hash (single-binding-table-sym bt)
        (single-binding-value (single-binding-table-scopes bt)
                              (single-binding-table-binding bt))))

;; Adding a binding for a computed-on-demand set of symbols
(define (binding-table-add-bulk bt scopes bulk)
  (cond
   [(table-with-bulk-bindings? bt)
    (table-with-bulk-bindings (remove-matching-bindings (table-with-bulk-bindings-syms bt)
                                                        scopes
                                                        bulk)
                              (cons (bulk-binding-at scopes bulk)
                                    (table-with-bulk-bindings-bulk-bindings bt)))]
   [else
    (define syms
      (cond
       [(not bt) #hasheq()]
       [(single-binding-table? bt) (single-to-hash bt)]
       [else bt]))
    (binding-table-add-bulk (table-with-bulk-bindings syms null) scopes bulk)]))

;; The bindings of `bulk at `scopes` should shadow any existing
;; mappings in `sym-bindings`
(define (remove-matching-bindings syms scopes bulk)
  (define bulk-symbols (bulk-binding-symbols bulk #f null))
  (cond
   [((hash-count syms) . < . (hash-count bulk-symbols))
    ;; Faster to consider each sym in `sym-binding`
    (for/fold ([syms syms]) ([(sym sym-bindings) (in-immutable-hash syms)])
      (if (hash-ref bulk-symbols sym #f)
          (remove-matching-binding syms sym sym-bindings scopes)
          syms))]
   [else
    ;; Faster to consider each sym in `bulk-symbols`
    (for/fold ([syms syms]) ([sym (in-immutable-hash-keys bulk-symbols)])
      (define sym-bindings (hash-ref syms sym #f))
      (if sym-bindings
          (remove-matching-binding syms sym sym-bindings scopes)
          syms))]))

;; Update an individual symbol's bindings to remove a mapping
;; for a given set of scopes
(define (remove-matching-binding syms sym sym-bindings scopes)
  (cond
   [(hash? sym-bindings)
    (hash-set syms sym (hash-remove sym-bindings scopes))]
   [(set=? scopes (single-binding-value-scopes sym-bindings))
    (hash-remove syms sym)]
   [else syms]))

;; ----------------------------------------

;; Iterate through all scope+
(define-sequence-syntax in-binding-table
  (lambda () #'do-not-use-in-binding-as-an-expression)
  (lambda (stx)
    (syntax-case stx ()
      [[(scopes-id binding-id) (_ sym-expr table-expr s-expr extra-shifts-expr)]
       #'[(scopes-id binding-id)
          (:do-in
           ([(sym table)
             (let ([sym sym-expr]
                   [table table-expr])
               (values sym
                       (cond
                        [(hash? table) (hash-ref table sym #f)]
                        [(single-binding-table? table)
                         (and (eq? sym (single-binding-table-sym table))
                              table)]
                        [else table])))]
            [(s) s-expr]
            [(extra-shifts) extra-shifts-expr])
           #t
           ([i (cond
                [(not table) #f]
                [(hash? table) (hash-iterate-first table)]
                [(table-with-bulk-bindings? table)
                 (define v (hash-ref (table-with-bulk-bindings-syms table) sym #f))
                 (or (cond
                      [(hash? v) (hash-iterate-first v)]
                      [else v])
                     (table-with-bulk-bindings-bulk-bindings table))]
                [else 'start])]
            [already-covered-scopes #hasheq()]) ; scope sets with bindings to overriding bulk bindings
           (cond
            [(single-binding-table? table) (eq? i 'start)]
            [(single-binding-value? table) (eq? i 'start)]
            [(table-with-bulk-bindings? table) (not (null? i))]
            [else i])
           ([(scopes-id) (cond
                          [(single-binding-table? table) (single-binding-table-scopes table)]
                          [(single-binding-value? table) (single-binding-value-scopes table)]
                          [(table-with-bulk-bindings? table)
                           (cond
                            [(single-binding-value? i) (single-binding-value-scopes i)]
                            [(integer? i)
                             (define ht (hash-ref (table-with-bulk-bindings-syms table) sym))
                             (hash-iterate-key ht i)]
                            [else
                             (define scs (bulk-binding-at-scopes (car i)))
                             (and (not (set-member? already-covered-scopes scs))
                                  scs)])]
                          [else (hash-iterate-key table i)])]
            [(binding-id) (cond
                           [(single-binding-table? table) (single-binding-table-binding table)]
                           [(single-binding-value? table) (single-binding-value-binding table)]
                           [(table-with-bulk-bindings? table)
                            (cond
                             [(single-binding-value? i) (single-binding-value-binding i)]
                             [(integer? i)
                              (define ht (hash-ref (table-with-bulk-bindings-syms table) sym))
                              (hash-iterate-value ht i)]
                             [else
                              (define bulk (bulk-binding-at-bulk (car i)))
                              (define b-info (hash-ref (bulk-binding-symbols bulk s extra-shifts) sym #f))
                              (and b-info
                                   ((bulk-binding-create bulk) bulk b-info sym))])]
                           [else (hash-iterate-value table i)])])
           #t
           #t
           [;; Next value for `i`:
            (cond
             [(single-binding-table? table) #f]
             [(single-binding-value? table) #f]
             [(table-with-bulk-bindings? table)
              (cond
               [(single-binding-value? i) (table-with-bulk-bindings-bulk-bindings table)]
               [(integer? i)
                (define ht (hash-ref (table-with-bulk-bindings-syms table) sym))
                (or (hash-iterate-next ht i)
                    (table-with-bulk-bindings-bulk-bindings table))]
               [else (cdr i)])]
             [(hash? table) (hash-iterate-next table i)])
            ;; Next value for `already-covered-scopes`; add the just-reported scope set
            ;; if we'll continue to explore bulk bindings
            (cond
             [(and (table-with-bulk-bindings? table)
                   scopes-id
                   binding-id
                   (or (not (pair? i)) (not (null? (cdr i)))))
              (set-add already-covered-scopes scopes-id)]
             [else already-covered-scopes])])]])))

;; ----------------------------------------

(define (binding-table-prune-to-reachable bt state)
  (or (hash-ref (serialize-state-bindings-intern state) bt #f)
      (let ([reachable-scopes (serialize-state-reachable-scopes state)])
        (define new-bt
          (cond
           [(not bt) #f]
           [(single-binding-table? bt)
            (if (subset? (single-binding-table-scopes bt) reachable-scopes)
                (struct-copy single-binding-table bt
                             [scopes (intern-scopes (single-binding-table-scopes bt) state)])
                #f)]
           [(hash? bt)
            (for*/hash ([(sym bindings-for-sym) (in-immutable-hash bt)]
                        [new-bindings-for-sym
                         (in-value
                          (cond
                           [(single-binding-value? bindings-for-sym)
                            (if (subset? (single-binding-value-scopes bindings-for-sym) reachable-scopes)
                                (struct-copy single-binding-value bindings-for-sym
                                             [scopes (intern-scopes (single-binding-value-scopes bindings-for-sym)
                                                                    state)])
                                #f)]
                           [else
                            (define new-bindings-for-sym
                              (for/hash ([(scopes binding) (in-immutable-hash bindings-for-sym)]
                                         #:when (subset? scopes reachable-scopes))
                                (values (intern-scopes scopes state) binding)))
                            (and (positive? (hash-count new-bindings-for-sym))
                                 new-bindings-for-sym)]))]
                        #:when new-bindings-for-sym)
              (values sym new-bindings-for-sym))]
           [else
            (define syms (binding-table-prune-to-reachable (table-with-bulk-bindings-syms bt) state))
            (define new-bulk-bindings
              (for/list ([bba (in-list (table-with-bulk-bindings-bulk-bindings bt))]
                         #:when (subset? (bulk-binding-at-scopes bba) reachable-scopes))
                (struct-copy bulk-binding-at bba
                             [scopes (intern-scopes (bulk-binding-at-scopes bba) state)])))
            (if (pair? new-bulk-bindings)
                (table-with-bulk-bindings syms new-bulk-bindings)
                syms)]))
        (hash-set! (serialize-state-bulk-bindings-intern state) bt new-bt)
        new-bt)))

(define (binding-table-register-reachable bt reachable-scopes reach register-trigger)
  (cond
   [(not bt) (void)]
   [(single-binding-table? bt)
    (scopes-register-reachable (single-binding-table-scopes bt)
                               (single-binding-table-binding bt)
                               reachable-scopes reach register-trigger)]
   [(hash? bt)
    (for ([(sym bindings-for-sym) (in-immutable-hash bt)])
      (cond
       [(single-binding-value? bindings-for-sym)
        (scopes-register-reachable (single-binding-value-scopes bindings-for-sym)
                                   (single-binding-value-binding bindings-for-sym)
                                   reachable-scopes reach register-trigger)]
       [else
        (for ([(scopes binding) (in-immutable-hash bindings-for-sym)])
          (scopes-register-reachable scopes binding reachable-scopes reach register-trigger))]))]
   [else
    (binding-table-register-reachable (table-with-bulk-bindings-syms bt) reachable-scopes reach register-trigger)]))

(define (scopes-register-reachable scopes binding reachable-scopes reach register-trigger)
  (define v (and (binding-reach-scopes? binding)
                 ((binding-reach-scopes-ref binding) binding)))
  (when v
    (cond
     [(subset? scopes reachable-scopes)
      (reach v)]
     [else
      (for ([sc (in-set scopes)]
            #:unless (set-member? reachable-scopes sc))
        (register-trigger sc v))])))
