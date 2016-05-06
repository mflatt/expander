#lang racket/base
(require racket/set
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "namespace.rkt")

(provide make-requires+provides
         
         (struct-out required)
         add-required-module!
         add-defined-or-required-id!
         check-required-or-defined
         extract-module-requires
         
         reset-provides!
         add-provide!
         
         attach-require-provide-properties)

;; ----------------------------------------

(struct requires+provides (requires   ; module-name-> require-phase -> list of (required id phase boolean)
                           provides)) ; phase -> sym -> binding

(define (make-requires+provides)
  (requires+provides (make-hash)
                     (make-hasheqv)))

;; ----------------------------------------

(struct required (id phase can-shadow?))

;; Register that a module is requires at a given phase shift
(define (add-required-module! r+p mod-name phase-shift)
  (hash-update! (requires+provides-requires r+p)
                mod-name
                (lambda (at-mod)
                  (hash-update at-mod
                               phase-shift
                               (lambda (l) l)
                               null))
                #hasheqv()))

;; Register a specific identifier that is required
(define (add-defined-or-required-id! r+p id phase binding
                                          #:can-shadow? [can-shadow? #f])
  ;; Register specific required identifier
  (unless (equal? phase (phase+ (module-binding-nominal-phase binding)
                                (module-binding-nominal-require-phase binding)))
    (error "internal error: binding phase does not match nominal info"))
  
  (hash-update! (requires+provides-requires r+p)
                (module-binding-nominal-module binding)
                (lambda (at-mod)
                  (hash-update at-mod
                               (module-binding-nominal-require-phase binding)
                               (lambda (l) (cons (required id phase can-shadow?) l))
                               null))
                #hasheqv()))

;; Check whether an identifier has a binding that is from a non-shadowable
;; require
(define (check-required-or-defined r+p id phase)
  (define b (resolve id phase #:exactly? #t))
  (when b
    (define at-mod (hash-ref (requires+provides-requires r+p)
                             (module-binding-nominal-module b)
                             #f))
    (and at-mod
         (for ([r (in-list (hash-ref at-mod
                                     (module-binding-nominal-require-phase b)
                                     null))])
           (when (and (eq? (syntax-e id) (syntax-e (required-id r)))
                      (not (required-can-shadow? r)))
             (error "already required or defined:" id))))))

;; Get All the bindings imported from a given module
(define (extract-module-requires r+p mod-path phase)
  (define at-mod (hash-ref (requires+provides-requires r+p) mod-path #f))
  (and at-mod
       (hash-ref at-mod phase #f)))

;; ----------------------------------------

;; Clear recorded provides
(define (reset-provides! r+p)
  (hash-clear! (requires+provides-provides r+p)))

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
                   [(and (equal? (module-binding-module b) (module-binding-module binding))
                         (eqv? (module-binding-phase b) (module-binding-phase binding))
                         (eq? (module-binding-sym b) (module-binding-sym binding)))
                    ;; If `binding` has different nomina info (i.e., same binding
                    ;; required from different syntactic sources), we keep only
                    ;; the first once.
                    at-phase]
                   [else
                    (error "name already provided as a different binding:" sym)]))
                #hasheq()))

;; ----------------------------------------

;; Extract both require and provide information into a syntax property
;; for use by `compile`
(define (attach-require-provide-properties r+p s self)
  (define (extract-requires)
    (define mht
      (for/fold ([mht #hasheqv()]) ([(module-name ht) (in-hash (requires+provides-requires r+p))])
        (for/fold ([mht mht]) ([phase (in-hash-keys ht)])
          (if (eq? module-name self)
              mht
              (hash-update mht phase (lambda (s) (set-add s module-name)) (set))))))
    (for/hasheqv ([(phase mods) (in-hash mht)])
      (values phase (set->list mods))))
  (let* ([s (syntax-property s 'module-requires (extract-requires))]
         [s (syntax-property s 'module-provides (requires+provides-provides r+p))])
    s))
