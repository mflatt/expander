#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/namespace.rkt"
         "../common/module-path.rkt")

(provide make-requires+provides
         requires+provides-self
         requires+provides-can-cross-phase-persistent?
         
         (struct-out required)
         add-required-module!
         add-defined-or-required-id!
         add-enclosing-module-defined-and-required!
         remove-required-id!
         check-not-defined
         add-defined-syms!
         extract-module-requires
         extract-module-definitions
         extract-all-module-requires
         
         reset-provides-and-defines!
         add-provide!
         
         attach-require-provide-properties
         
         shift-provides-module-path-index)

;; ----------------------------------------

(struct requires+provides (self       ; module-path-index to recognize definitions among requires
                           require-mpis ; module-path-index to itself as interned
                           require-mpis-in-order ; require-phase -> list of module-path-index
                           requires   ; module-path-index -> require-phase -> list of (required id phase boolean)
                           provides   ; phase -> sym -> binding
                           phase-to-defined-syms ; phase -> sym -> boolean
                           [can-cross-phase-persistent? #:mutable]))

(define (make-requires+provides self)
  (requires+provides self
                     (make-hash)    ; require-mpis
                     (make-hasheqv) ; require-mpis-in-order
                     (make-hash)    ; requires
                     (make-hasheqv) ; provides
                     (make-hasheqv) ; phase-to-defined-syms
                     #t))

;; ----------------------------------------

(struct required (id phase can-shadow?))

;; Register that a module is required at a given phase shift, and return a
;; locally interned module path index
(define (add-required-module! r+p mod-name phase-shift is-cross-phase-persistent?)
  (define mpi (or (hash-ref (requires+provides-require-mpis r+p)
                            mod-name
                            #f)
                  (begin
                    (hash-set! (requires+provides-require-mpis r+p) mod-name mod-name)
                    mod-name)))
  (unless (hash-ref (hash-ref (requires+provides-requires r+p) mpi #hasheqv()) phase-shift #f)
    ;; Add to list of requires that are kept in order, so that order
    ;; is preserved on instantiation
    (hash-update! (requires+provides-require-mpis-in-order r+p)
                  phase-shift
                  (lambda (l) (cons mpi l))
                  null)
    ;; Init list of required identifiers:
    (hash-update! (requires+provides-requires r+p)
                  mpi
                  (lambda (at-mod) (hash-set at-mod phase-shift null))
                  #hasheqv()))
  (unless is-cross-phase-persistent?
    (set-requires+provides-can-cross-phase-persistent?! r+p #f))
  mpi)

;; Register a specific identifier that is required
(define (add-defined-or-required-id! r+p id phase binding
                                          #:can-shadow? [can-shadow? #f])
  ;; Register specific required identifier
  (unless (equal? phase (phase+ (module-binding-nominal-phase binding)
                                (module-binding-nominal-require-phase binding)))
    (error "internal error: binding phase does not match nominal info"))
   (add-defined-or-required-id-at-nominal! r+p id phase
                                           #:nominal-module (module-binding-nominal-module binding)
                                           #:nominal-require-phase (module-binding-nominal-require-phase binding)
                                           #:can-shadow? can-shadow?))

;; The internals of `add-defined-or-required-id!` that consumes just
;; the needed part of the binding
(define (add-defined-or-required-id-at-nominal! r+p id phase
                                                #:nominal-module nominal-module
                                                #:nominal-require-phase nominal-require-phase
                                                #:can-shadow? can-shadow?)
  (hash-update! (requires+provides-requires r+p)
                nominal-module
                (lambda (at-mod)
                  (hash-update at-mod
                               nominal-require-phase
                               (lambda (l) (cons (required id phase can-shadow?) l))
                               null))
                #hasheqv()))

;; Add bindings of an enclosing module
(define (add-enclosing-module-defined-and-required! r+p
                                                    #:enclosing-requires+provides enclosing-r+p
                                                    enclosing-mod
                                                    phase-shift)
  (for ([(mod-name at-mod) (in-hash (requires+provides-requires enclosing-r+p))])
    (for* ([(phase at-phase) (in-hash at-mod)]
           [reqd (in-list at-phase)])
      (add-defined-or-required-id-at-nominal! r+p
                                              (syntax-module-path-index-shift
                                               (required-id reqd)
                                               (requires+provides-self enclosing-r+p)
                                               enclosing-mod)
                                              (phase+ (required-phase reqd) phase-shift)
                                              #:nominal-module enclosing-mod
                                              #:nominal-require-phase phase-shift
                                              #:can-shadow? #t))))

;; Removes a required identifier, in anticiation of it being defined
(define (remove-required-id! r+p id phase)
  (define b (resolve+shift id phase #:exactly? #t))
  (when b
    (hash-update! (requires+provides-requires r+p)
                  (module-binding-nominal-module b)
                  (lambda (at-mod)
                    (hash-set at-mod
                              (module-binding-nominal-require-phase b)
                              null))
                  #hasheqv())))

;; Check whether an identifier has a binding that is from a non-shadowable
;; require
(define (check-not-defined #:check-not-required? [check-not-required? #f]
                           r+p id phase #:in orig-s
                           #:unless-matches [ok-binding #f])
  (define b (resolve+shift id phase #:exactly? #t))
  (when (and b (not (module-binding? b)))
    (raise-syntax-error #f "identifier out of context" id))
  (define defined? (and b (eq? (requires+provides-self r+p)
                               (module-binding-module b))))
  (when (and b
             (or check-not-required?
                 (and defined?
                      ;; In case `#%module-begin` is expanded multiple times, check
                      ;; that the definition has been seen this particular expansion
                      (hash-ref (hash-ref (requires+provides-phase-to-defined-syms r+p)
                                          phase
                                          #hasheq())
                                (module-binding-sym b)
                                #f))))
    (define at-mod (hash-ref (requires+provides-requires r+p)
                             (module-binding-nominal-module b)
                             #f))
    (and at-mod
         (for ([r (in-list (hash-ref at-mod
                                     (module-binding-nominal-require-phase b)
                                     null))])
           (when (and (eq? (syntax-e id) (syntax-e (required-id r)))
                      (not (required-can-shadow? r))
                      (or (not ok-binding)
                          (not (same-binding? b ok-binding))))
             (raise-syntax-error #f
                                 (string-append "identifier already "
                                                (if defined? "defined" "required"))
                                 orig-s
                                 id))))))

(define (add-defined-syms! r+p syms phase)
  (define phase-to-defined-syms (requires+provides-phase-to-defined-syms r+p))
  (define defined-syms (hash-ref phase-to-defined-syms phase #hasheq()))
  (define new-defined-syms
    (for/fold ([defined-syms defined-syms]) ([sym (in-list syms)])
      (hash-set defined-syms sym #t)))
  (hash-set! phase-to-defined-syms phase new-defined-syms))

;; Get all the bindings imported from a given module
(define (extract-module-requires r+p mod-name phase)
  (define at-mod (hash-ref (requires+provides-requires r+p) mod-name #f))
  (and at-mod
       (hash-ref at-mod phase #f)))

;; Get all the definitions
(define (extract-module-definitions r+p)
  (extract-module-requires r+p (requires+provides-self r+p) 0))

;; Like `extract-module-requires`, but merging modules and phases
(define (extract-all-module-requires r+p
                                     mod-name ; or #f for "all"
                                     phase)   ; or 'all for "all"
  (define self (requires+provides-self r+p))
  (define requires (requires+provides-requires r+p))
  (for*/list ([mod-name (in-list (if mod-name
                                     (list mod-name)
                                     (hash-keys requires)))]
              #:unless (eq? mod-name self)
              [phase-to-requireds (in-value (hash-ref requires mod-name #hasheqv()))]
              [phase (in-list (if (eq? phase 'all)
                                  (hash-keys phase-to-requireds)
                                  (list phase)))]
              [reqd (in-list (hash-ref phase-to-requireds phase null))])
    reqd))

;; ----------------------------------------

;; Clear recorded provides
(define (reset-provides-and-defines! r+p)
  (hash-clear! (requires+provides-provides r+p))
  (hash-clear! (requires+provides-phase-to-defined-syms r+p)))

;; Register that a binding is provided as a given symbol; report an
;; error if the provide is inconsistent with an earlier one
(define (add-provide! r+p sym phase binding id)
  (hash-update! (requires+provides-provides r+p)
                phase
                (lambda (at-phase)
                  (define b (hash-ref at-phase sym #f))
                  (cond
                   [(not b)
                    (hash-set at-phase sym binding)]
                   [(and (eq? (module-path-index-resolve (module-binding-module b))
                              (module-path-index-resolve (module-binding-module binding)))
                         (eqv? (module-binding-phase b) (module-binding-phase binding))
                         (eq? (module-binding-sym b) (module-binding-sym binding)))
                    ;; If `binding` has different nominal info (i.e., same binding
                    ;; required from different syntactic sources), we keep only
                    ;; the first once.
                    at-phase]
                   [else
                    (error "name already provided as a different binding:" sym
                           b binding)]))
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
    (shift-provides-module-path-index (provides-forward-free=id
                                       (requires+provides-provides r+p))
                                      old-self
                                      new-self))
  (let* ([s (syntax-property s 'module-requires (extract-requires))]
         [s (syntax-property s 'module-provides (extract-provides))])
    s))

;; For each binding, flatten `free-identifier=?` equivalences
;; installed by rename transformers
(define (provides-forward-free=id provides)
  (for/hasheqv ([(phase at-phase) (in-hash provides)])
    (values phase
            (for/hasheq ([(sym binding) (in-hash at-phase)])
              (values sym
                      (let loop ([binding binding])
                        (cond
                         [(binding-free=id binding)
                          (loop (binding-free=id binding))]
                         [else binding])))))))

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
                        (binding-module-path-index-shift binding from-mpi to-mpi)))))]))
