#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/namespace.rkt"
         "../namespace/provided.rkt"
         "../common/module-path.rkt")

(provide make-requires+provides
         requires+provides-self
         requires+provides-can-cross-phase-persistent?
         
         requires+provides-all-bindings-simple?
         set-requires+provides-all-bindings-simple?!
         
         (struct-out required)
         add-required-module!
         add-defined-or-required-id!
         add-bulk-required-ids!
         add-enclosing-module-defined-and-required!
         remove-required-id!
         check-not-defined
         add-defined-syms!
         extract-module-requires
         extract-module-definitions
         extract-all-module-requires
         
         requires+provides-reset!
         add-provide!
         
         attach-require-provide-properties
         
         shift-provides-module-path-index)

;; ----------------------------------------

(struct requires+provides (self       ; module-path-index to recognize definitions among requires
                           require-mpis ; module-path-index to itself as interned
                           require-mpis/fast ; same table, but `eq?`-keyed for fast already-interned checks
                           require-mpis-in-order ; require-phase -> list of module-path-index
                           requires   ; mpi [interned] -> require-phase -> sym -> list of [bulk-]required
                           provides   ; phase -> sym -> binding or protected
                           phase-to-defined-syms ; phase -> sym -> boolean
                           [can-cross-phase-persistent? #:mutable]
                           [all-bindings-simple? #:mutable])) ; tracks whether bindings are easily reconstructed

;; A `required` represents an identifier required into a module
(struct required (id phase can-be-shadowed? as-transformer?))

;; A `bulk-required` can be converted into a `required` given the
;; module path, phase, and symbol that are mapped to it
(struct bulk-required (provides ; extract binding info based on the sym
                       s        ; combine with the sym to create an identifier
                       self     ; source of module path index shift to the one that reaches the bulk require
                       provide-phase-level ; phase of `provide` in immediately providing module
                       can-be-shadowed?))  ; shadowed because, e.g., an initial import

(define (make-requires+provides self)
  (requires+provides self
                     (make-hash)    ; require-mpis
                     (make-hasheq)  ; require-mpis/fast
                     (make-hasheqv) ; require-mpis-in-order
                     (make-hasheq)  ; requires
                     (make-hasheqv) ; provides
                     (make-hasheqv) ; phase-to-defined-syms
                     #t
                     #t))

(define (requires+provides-reset! r+p)
  (hash-clear! (requires+provides-require-mpis-in-order r+p))
  (hash-clear! (requires+provides-requires r+p))
  (hash-clear! (requires+provides-provides r+p))
  (hash-clear! (requires+provides-phase-to-defined-syms r+p)))

;; ----------------------------------------

(define (intern-mpi r+p mpi)
  (or (hash-ref (requires+provides-require-mpis/fast r+p)
                mpi
                #f)
      (hash-ref (requires+provides-require-mpis r+p)
                mpi
                #f)
      (begin
        (hash-set! (requires+provides-require-mpis r+p) mpi mpi)
        (hash-set! (requires+provides-require-mpis/fast r+p) mpi mpi)
        mpi)))

;; ----------------------------------------

;; Register that a module is required at a given phase shift, and return a
;; locally interned module path index
(define (add-required-module! r+p mod-name phase-shift is-cross-phase-persistent?)
  (define mpi (intern-mpi r+p mod-name))
  (unless (hash-ref (hash-ref (requires+provides-requires r+p) mpi #hasheqv()) phase-shift #f)
    ;; Add to list of requires that are kept in order, so that order
    ;; is preserved on instantiation
    (hash-update! (requires+provides-require-mpis-in-order r+p)
                  phase-shift
                  (lambda (l) (cons mpi l))
                  null)
    ;; Init list of required identifiers:
    (hash-set! (hash-ref! (requires+provides-requires r+p) mpi make-hasheqv)
               phase-shift
               (make-hasheq)))
  (unless is-cross-phase-persistent?
    (set-requires+provides-can-cross-phase-persistent?! r+p #f))
  mpi)

;; Register a specific identifier that is required
(define (add-defined-or-required-id! r+p id phase binding
                                          #:can-be-shadowed? [can-be-shadowed? #f]
                                          #:as-transformer? as-transformer?)
  ;; Register specific required identifier
  (unless (equal? phase (phase+ (module-binding-nominal-phase binding)
                                (module-binding-nominal-require-phase binding)))
    (error "internal error: binding phase does not match nominal info"))
  (add-defined-or-required-id-at-nominal! r+p id phase
                                          #:nominal-module (module-binding-nominal-module binding)
                                          #:nominal-require-phase (module-binding-nominal-require-phase binding)
                                          #:can-be-shadowed? can-be-shadowed?
                                          #:as-transformer? as-transformer?))

;; The internals of `add-defined-or-required-id!` that consumes just
;; the needed part of the binding
(define (add-defined-or-required-id-at-nominal! r+p id phase
                                                #:nominal-module nominal-module
                                                #:nominal-require-phase nominal-require-phase
                                                #:can-be-shadowed? can-be-shadowed?
                                                #:as-transformer? as-transformer?)
  (define at-mod (hash-ref! (requires+provides-requires r+p)
                            (intern-mpi r+p nominal-module)
                            make-hasheqv))
  (define sym-to-reqds (hash-ref! at-mod nominal-require-phase make-hasheq))
  (define sym (syntax-e id))
  (hash-set! sym-to-reqds sym (cons (required id phase can-be-shadowed? as-transformer?)
                                    (hash-ref sym-to-reqds sym null))))

;; Like `add-defined-or-required-id!`, but faster for bindings that
;; all have the same scope, etc.
(define (add-bulk-required-ids! r+p s self nominal-module phase-shift provides provide-phase-level
                                #:can-be-shadowed? can-be-shadowed?)
  (define at-mod (hash-ref! (requires+provides-requires r+p)
                            (intern-mpi r+p nominal-module)
                            make-hasheqv))
  (define sym-to-reqds (hash-ref! at-mod phase-shift make-hasheq))
  (define br (bulk-required provides s self provide-phase-level can-be-shadowed?))
  (for ([sym (in-hash-keys provides)])
    (hash-set! sym-to-reqds sym (cons br (hash-ref sym-to-reqds sym null)))))

;; Convert a combination of a symbol and `bulk-required` to a
;; `required` on demand
(define (bulk-required->required br nominal-module phase sym)
  (define binding/p (hash-ref (bulk-required-provides br) sym))
  (required (datum->syntax (bulk-required-s br) sym)
            (phase+ phase (bulk-required-provide-phase-level br))
            (bulk-required-can-be-shadowed? br)
            (provided-as-transformer? binding/p)))

(define (normalize-required r mod-name phase sym)
  (if (bulk-required? r)
      (bulk-required->required r mod-name phase sym)
      r))

;; Add bindings of an enclosing module
(define (add-enclosing-module-defined-and-required! r+p
                                                    #:enclosing-requires+provides enclosing-r+p
                                                    enclosing-mod
                                                    phase-shift)
  (set-requires+provides-all-bindings-simple?! r+p #f)
  (for ([(mod-name at-mod) (in-hash (requires+provides-requires enclosing-r+p))])
    (for* ([(phase at-phase) (in-hash at-mod)]
           [(sym reqds) (in-hash at-phase)]
           [reqd/maybe-bulk (in-list reqds)])
      (define reqd (normalize-required reqd/maybe-bulk mod-name phase sym))
      (add-defined-or-required-id-at-nominal! r+p
                                              (syntax-module-path-index-shift
                                               (required-id reqd)
                                               (requires+provides-self enclosing-r+p)
                                               enclosing-mod)
                                              (phase+ (required-phase reqd) phase-shift)
                                              #:nominal-module enclosing-mod
                                              #:nominal-require-phase phase-shift
                                              #:can-be-shadowed? #t
                                              #:as-transformer? (required-as-transformer? reqd)))))

;; Removes a required identifier, in anticipation of it being defined
(define (remove-required-id! r+p id phase #:unless-matches binding)
  (define b (resolve+shift id phase #:exactly? #t))
  (when b
    (define mpi (intern-mpi r+p (module-binding-nominal-module b)))
    (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
    (when at-mod
      (define nominal-phase (module-binding-nominal-require-phase b))
      (define sym-to-reqds (hash-ref at-mod
                                     nominal-phase
                                     #f))
      (when sym-to-reqds
        (define sym (syntax-e id))
        (define l (hash-ref sym-to-reqds sym null))
        (unless (null? l)
          (unless (same-binding? b binding)
            (hash-set! sym-to-reqds sym
                       (for*/list ([r (in-list l)]
                                   [r (in-value (normalize-required r mpi nominal-phase sym))]
                                   #:unless (free-identifier=? (required-id r) id phase phase))
                         r))))))))

;; Check whether an identifier has a binding that is from a non-shadowable
;; require; if something is found but it will be replaced, then record that
;; bindings are not simple.
(define (check-not-defined #:check-not-required? [check-not-required? #f]
                           r+p id phase #:in orig-s
                           #:unless-matches [ok-binding #f])
  (define b (resolve+shift id phase #:exactly? #t))
  (cond
   [(not b) (void)]
   [(not (module-binding? b))
    (raise-syntax-error #f "identifier out of context" id)]
   [else
    (define defined? (and b (eq? (requires+provides-self r+p)
                                 (module-binding-module b))))
    (cond
     [(and (not defined?) (not check-not-required?))
      ;; Not defined, and we're shadowing all requires -- so, it's ok,
      ;; but binding is non-simple
      (set-requires+provides-all-bindings-simple?! r+p #f)]
     [(and defined?
           ;; In case `#%module-begin` is expanded multiple times, check
           ;; that the definition has been seen this particular expansion
           (not (hash-ref (hash-ref (requires+provides-phase-to-defined-syms r+p)
                                    phase
                                    #hasheq())
                          (module-binding-sym b)
                          #f)))
      ;; Doesn't count as previously defined
      (void)]
     [else
      (define at-mod (hash-ref (requires+provides-requires r+p)
                               (intern-mpi r+p (module-binding-nominal-module b))
                               #f))
      (cond
       [(not at-mod)
        ;; Binding is from an enclosing context; if it's from an
        ;; enclosing module, then we've already marked bindings
        ;; a non-simple --- otherwise, we don't care
        (void)]
       [(and ok-binding (same-binding? b ok-binding))
        ;; It's the same binding already, so overall binding hasn't
        ;; become non-simple
        (void)]
       [else
        (for ([r (in-list (hash-ref (hash-ref at-mod
                                              (module-binding-nominal-require-phase b)
                                              #hasheq())
                                    (syntax-e id)
                                    null))])
          (cond
           [(if (bulk-required? r)
                (bulk-required-can-be-shadowed? r)
                (required-can-be-shadowed? r))
            ;; Shadowing --- ok, but non-simple
            (set-requires+provides-all-bindings-simple?! r+p #f)]
           [else
            (raise-syntax-error #f
                                (string-append "identifier already "
                                               (if defined? "defined" "required")
                                               (cond
                                                [(zero-phase? phase) ""]
                                                [(label-phase? phase) " for label"]
                                                [(= 1 phase) " for syntax"]
                                                [else (format " for phase ~a" phase)]))
                                orig-s
                                id)]))])])]))

(define (add-defined-syms! r+p syms phase)
  (define phase-to-defined-syms (requires+provides-phase-to-defined-syms r+p))
  (define defined-syms (hash-ref phase-to-defined-syms phase #hasheq()))
  (define new-defined-syms
    (for/fold ([defined-syms defined-syms]) ([sym (in-list syms)])
      (hash-set defined-syms sym #t)))
  (hash-set! phase-to-defined-syms phase new-defined-syms))

;; Get all the bindings imported from a given module
(define (extract-module-requires r+p mod-name phase)
  (define mpi (intern-mpi r+p mod-name))
  (define at-mod (hash-ref (requires+provides-requires r+p) mpi #f))
  (and at-mod
       (for*/list ([(sym reqds) (in-hash (hash-ref at-mod phase #hasheq()))]
                   [reqd (in-list reqds)])
         (normalize-required reqd mpi phase sym))))

;; Get all the definitions
(define (extract-module-definitions r+p)
  (or (extract-module-requires r+p (requires+provides-self r+p) 0)
      null))

;; Like `extract-module-requires`, but merging modules and phases
(define (extract-all-module-requires r+p
                                     mod-name ; or #f for "all"
                                     phase)   ; or 'all for "all"
  (define self (requires+provides-self r+p))
  (define requires (requires+provides-requires r+p))
  (let/ec esc
    (for*/list ([mod-name (in-list (if mod-name
                                       (list (intern-mpi r+p mod-name))
                                       (hash-keys requires)))]
                #:unless (eq? mod-name self)
                [phase-to-requireds (in-value (hash-ref requires mod-name #hasheqv()))]
                [phase (in-list (if (eq? phase 'all)
                                    (hash-keys phase-to-requireds)
                                    (list phase)))]
                [(sym reqds) (in-hash
                              (hash-ref phase-to-requireds phase
                                        ;; failure => not required at that phase
                                        (lambda () (esc #f))))]
                [reqd (in-list reqds)])
      (normalize-required reqd mod-name phase sym))))

;; ----------------------------------------

;; Register that a binding is provided as a given symbol; report an
;; error if the provide is inconsistent with an earlier one
(define (add-provide! r+p sym phase binding immed-binding id orig-s
                      #:as-protected? as-protected?
                      #:as-transformer? as-transformer?)
  (when (and as-protected?
             (not (eq? (module-binding-module immed-binding) (requires+provides-self r+p))))
    (raise-syntax-error #f "cannot protect imported identifier with re-provide" sym))
  (hash-update! (requires+provides-provides r+p)
                phase
                (lambda (at-phase)
                  (define b/p (hash-ref at-phase sym #f))
                  (define b (provided-as-binding b/p))
                  (cond
                   [(not b)
                    (hash-set at-phase sym (if (or as-protected? as-transformer?)
                                               (provided binding as-protected? as-transformer?)
                                               binding))]
                   [(same-binding? b binding)
                    ;; If `binding` has different nominal info (i.e., same binding
                    ;; required from different syntactic sources), we keep only
                    ;; the first once.
                    at-phase]
                   [else
                    (raise-syntax-error #f
                                        "identifier already provided (as a different binding)"
                                        orig-s id)]))
                #hasheq()))

;; ----------------------------------------

;; Extract both require and provide information into a syntax property
;; for use by `compile`
(define (attach-require-provide-properties r+p s old-self new-self)
  (define (extract-requires)
    ;; Extract from the in-order record, so that instantiation can use the original order
    (define phase-to-mpis-in-order (requires+provides-require-mpis-in-order r+p))
    (define phases-in-order (sort (hash-keys phase-to-mpis-in-order) phase<?))
    (for/list ([phase (in-list phases-in-order)])
      (cons phase
            (for/list ([mpi (in-list (reverse (hash-ref phase-to-mpis-in-order phase)))]
                       #:unless (eq? mpi old-self))
              (module-path-index-shift mpi old-self new-self)))))
  (define (extract-provides)
    (shift-provides-module-path-index (requires+provides-provides r+p)
                                      old-self
                                      new-self))
  (let* ([s (syntax-property s 'module-requires (extract-requires))]
         [s (syntax-property s 'module-provides (extract-provides))])
    s))

;; ----------------------------------------

(define (shift-requires-module-path-index requires from-mpi to-mpi)
  (cond
   [(eq? from-mpi to-mpi) requires]
   [else
    (for/hash ([(phase mpis) (in-hash requires)])
      (values phase
              (for/list ([mpi (in-list mpis)])
                (module-path-index-shift mpi from-mpi to-mpi))))]))

(define (shift-provides-module-path-index provides from-mpi to-mpi)
  (cond
   [(eq? from-mpi to-mpi) provides]
   [else
    (for/hasheqv ([(phase at-phase) (in-hash provides)])
      (values phase
              (for/hasheq ([(sym binding) (in-hash at-phase)])
                (values sym
                        (let loop ([binding binding])
                          (cond
                           [(provided? binding)
                            (provided (loop (provided-binding binding))
                                      (provided-protected? binding)
                                      (provided-syntax? binding))]
                           [else
                            (binding-module-path-index-shift binding from-mpi to-mpi)]))))))]))
