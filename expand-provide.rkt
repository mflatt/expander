#lang racket/base
(require racket/set
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "match.rkt"
         "binding.rkt"
         "require.rkt"
         "expand-context.rkt"
         "core.rkt"
         "module-path.rkt")

(provide parse-and-expand-provides!
         
         make-require-provide-registry
         reset-provides!
         register-defined-or-required-id!
         attach-require-provide-properties)

(define layers '(raw phaseless id))

(define (parse-and-expand-provides! specs
                                    rp self
                                    phase ctx
                                    expand rebuild)
  ;; returns a list of expanded specs while registering provides in `rp`
  (let loop ([specs specs]
             [at-phase phase]
             [protected? #f]
             [layer 'raw])
    (apply
     append
     (for/list ([spec (in-list specs)])
       (define (check-nested want-layer)
         (unless (member want-layer (member layer layers))
           (error "invalid nesting:" spec)))
       (define fm (and (pair? (syntax-e spec))
                       (identifier? (car (syntax-e spec)))
                       (syntax-e (car (syntax-e spec)))))
       (case fm
         [(for-meta)
          (check-nested 'raw)
          (define m (match-syntax spec '(for-meta phase-level spec ...)))
          (define p (syntax-e (m 'phase-level)))
          (unless (phase? p)
            (error "bad phase:" spec))
          (list
           (rebuild
            spec
            `(,(m 'for-meta) ,(m 'phase-level) ,@(loop (m 'spec)
                                                       (phase+ p at-phase)
                                                       protected?
                                                       'phaseless))))]
         [(for-syntax)
          (check-nested 'raw)
          (define m (match-syntax spec '(for-syntax spec ...)))
          (rebuild
           spec
           `(,(m 'for-syntax) ,@(loop (m 'spec)
                                      (phase+ 1 at-phase)
                                      protected?
                                      'phaseless)))]
         [(for-label)
          (check-nested 'raw)
          (define m (match-syntax spec '(for-label spec ...)))
          (list
           (rebuild spec `(,(m 'for-label) ,@(loop (m 'spec)
                                                   #f
                                                   protected?
                                                   'phaseless))))]
         [(protect)
          (check-nested 'phaseless)
          (when protected?
            (error "invalid nesting:" spec))
          (define m (match-syntax spec '(protect spec ...)))
          (list
           (rebuild spec `(,(m 'protect) ,@(loop (m 'spec)
                                                 at-phase
                                                 #t
                                                 layer))))]
         [(rename)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(rename id:from id:to)))
          (parse-identifier! (m 'id:from) (syntax-e (m 'id:to)) at-phase rp)
          (list spec)]
         [(struct)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(struct id:struct (id:field ...))))
          (parse-struct! (m 'id:struct) (m 'id:field) at-phase rp)
          (list spec)]
         [(all-from)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(all-from mod-path)))
          (parse-all-from (m 'mod-path) null at-phase rp)
          (list spec)]
         [(all-from-except)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(all-from-except mod-path id ...)))
          (parse-all-from (m 'mod-path) (m 'id) at-phase rp)
          (list spec)]
         [(all-defined)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(all-defined)))
          (parse-all-from-module self spec null #f at-phase rp)
          (list spec)]
         [(all-defined-except)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(all-defined-except id ...)))
          (parse-all-from-module self spec (m 'id) #f at-phase rp)
          (list spec)]
         [(prefix-all-defined)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(prefix-all-defined id:prefix)))
          (parse-all-from-module self spec null (syntax-e (m 'id:prefix)) at-phase rp)
          (list spec)]
         [(prefix-all-defined-except)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(prefix-all-defined-except id:prefix id ...)))
          (parse-all-from-module self spec (m 'id) (syntax-e (m 'id:prefix)) at-phase rp)
          (list spec)]
         [(expand)
          (void (match-syntax spec '(expand (id . datum))))
          (define m (match-syntax spec '(expand form)))
          (define exp-spec (expand (m 'form) (struct-copy expand-context ctx
                                                          [phase phase])))
          (unless (and (pair? (syntax-e exp-spec))
                       (identifier? (car (syntax-e exp-spec)))
                       (eq? 'begin (core-form-sym (car (syntax-e exp-spec)))))
            (error "expansion of `provide` spec does not start `begin`:" spec))
          (define e-m (match-syntax exp-spec '(begin spec ...)))
          (loop (e-m 'spec)
                at-phase
                protected?
                layer)]
         [else
          (cond
           [(identifier? spec)
            (parse-identifier! spec (syntax-e spec) at-phase rp)
            (list spec)]
           [else
            (error "bad provide spec:" spec)])])))))

;; ----------------------------------------

(define (parse-identifier! spec sym at-phase rp)
  (define b (resolve spec at-phase))
  (unless b
    (error "provided identifier is not defined or required:" spec))
  (register-provide! rp sym at-phase b spec))

(define (parse-struct! id:struct fields at-phase rp)
  (define (mk fmt)
    (define sym (string->symbol (format fmt (syntax-e id:struct))))
    (datum->syntax id:struct sym id:struct))
  (define (mk2 fmt field-id)
    (define sym (string->symbol (format fmt
                                        (syntax-e id:struct)
                                        (syntax-e field-id))))
    (datum->syntax id:struct sym id:struct))
  (for ([fmt (list* (mk "~a")
                    (mk "make-~a")
                    (mk "struct:~a")
                    (mk "~a?"))])
    (define id (mk fmt))
    (parse-identifier! id (syntax-e id) at-phase rp))
  (for ([field (in-list fields)])
    (define get-id (mk2 "~a-~a" field))
    (define set-id (mk2 "set-~a-~a!" field))
    (parse-identifier! get-id (syntax-e get-id) at-phase rp)
    (parse-identifier! set-id (syntax-e set-id) at-phase rp)))
  
(define (parse-all-from mod-path-stx except-ids at-phase rp)
  (define mod-path (syntax->datum mod-path-stx))
  (unless (module-path? mod-path)
    (error "not a module path:" mod-path-stx))
  (parse-all-from-module (resolve-module-path mod-path) #f except-ids #f at-phase rp))
  
(define (parse-all-from-module mod-path matching-stx except-ids prefix-sym at-phase rp)
  (define requireds (register-extract-module-requires rp mod-path at-phase))
  (unless requireds
    (error "no requires from module path:" mod-path "at phase:" at-phase))
  
  (define (add-prefix sym)
    (if prefix-sym
        (string->symbol (format "~a~a" prefix-sym sym))
        sym))

  (define found (make-hasheq))
  
  ;; Register all except excluded bindings:
  (for ([i (in-list requireds)])
    (define id (required-id i))
    (define phase (required-phase i))
    (unless (or (and matching-stx
                     ;; For `(all-defined-out)`, binding context must match:
                     (not (free-identifier=? id
                                             (datum->syntax matching-stx (syntax-e id))
                                             phase)))
                (for/or ([except-id (in-list except-ids)])
                  (and (free-identifier=? id except-id phase)
                       (hash-set! found except-id #t))))
      (register-provide! rp (add-prefix (syntax-e id)) phase (resolve id phase) id)))
  
  ;; Check that all exclusions matched something to exclude:
  (unless (= (hash-count found) (length except-ids))
    (for ([except-id (in-list except-ids)])
      (unless (or (hash-ref found except-id #f)
                  (for/or ([i (in-list requireds)])
                    (define id (required-id i))
                    (define phase (required-phase i))
                    (free-identifier=? id except-id phase)))
        (error (if matching-stx
                   "excluded identifier was not defined in the module:"
                   "excluded identifier was not required from the module:")
               except-id)))))

;; ----------------------------------------

(struct required (id phase))

(struct require-provide (module-to-ids       ; resolved-module-name-> require-phase -> list of (required id phase)
                         phase-to-provides)) ; phase -> sym -> binding

(define (make-require-provide-registry)
  (require-provide (make-hash)
                   (make-hasheqv)))

(define (reset-provides! rp)
  (hash-clear! (require-provide-phase-to-provides rp)))

(define (register-defined-or-required-id! rp id phase binding)
  (unless (equal? phase (phase+ (module-binding-nominal-phase binding)
                                (module-binding-nominal-require-phase binding)))
    (error "internal error: binding phase does not match nominal info"))
  
  (hash-update! (require-provide-module-to-ids rp)
                (module-binding-nominal-module binding)
                (lambda (at-mod)
                  (hash-update at-mod
                               (module-binding-nominal-require-phase binding)
                               (lambda (l) (cons (required id phase) l))
                               null))
                #hasheqv()))

(define (register-provide! rp sym phase binding id)
  (hash-update! (require-provide-phase-to-provides rp)
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

(define (register-extract-module-requires rp mod-path phase)
  (define at-mod (hash-ref (require-provide-module-to-ids rp) mod-path #f))
  (and at-mod
       (hash-ref at-mod phase #f)))

(define (attach-require-provide-properties s rp self)
  (define (extract-requires)
    (define mht
      (for/fold ([mht #hasheqv()]) ([(module-name ht) (in-hash (require-provide-module-to-ids rp))])
        (for/fold ([mht mht]) ([phase (in-hash-keys ht)])
          (if (eq? module-name self)
              mht
              (hash-update mht phase (lambda (s) (set-add s module-name)) (set))))))
    (for/hasheqv ([(phase mods) (in-hash mht)])
      (values phase (set->list mods))))
  (let* ([s (syntax-property s 'module-requires (extract-requires))]
         [s (syntax-property s 'module-provides (require-provide-phase-to-provides rp))])
    s))
