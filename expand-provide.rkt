#lang racket/base
(require racket/set
         "stx.rkt"
         "scope.rkt"
         "pattern.rkt"
         "binding.rkt"
         "require.rkt"
         "module-path.rkt")

(provide parse-and-expand-provides!
         
         make-import-export-registry
         register-defined-or-imported-id!)

(define layers '(raw phaseless id))

(define (parse-and-expand-provides! specs
                                    ie self
                                    phase ctx
                                    expand rebuild)
  ;; returns a list of expanded specs while registering exports in `ie`
  (let loop ([specs specs]
             [at-phase 0]
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
          (define m (parse-syntax spec '(for-meta phase-level spec ...)))
          (define p (syntax-e (m 'phase-level)))
          (unless (phase? p)
            (error "bad phase:" spec))
          (rebuild
           spec
           `(,(m 'for-meta) ,(m 'phase-level) ,@(loop (m 'spec)
                                                      p
                                                      protected?
                                                      'phaseless)))]
         [(for-syntax)
          (check-nested 'raw)
          (define m (parse-syntax spec '(for-syntax spec ...)))
          (rebuild
           spec
           `(,(m 'for-syntax) ,@(loop (m 'spec)
                                      1
                                      protected?
                                      'phaseless)))]
         [(for-label)
          (check-nested 'raw)
          (define m (parse-syntax spec '(for-label spec ...)))
          (rebuild spec `(,(m 'for-label) ,@(loop (m 'spec)
                                                  #f
                                                  protected?
                                                  'phaseless)))]
         [(protect)
          (check-nested 'phaseless)
          (when protected?
            (error "invalid nesting:" spec))
          (define m (parse-syntax spec '(protect spec ...)))
          (rebuild spec `(,(m 'protect) ,@(loop (m 'spec)
                                                at-phase
                                                #t
                                                layer)))]
         [(rename)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(rename id:<from id:to)))
          (parse-identifier! (m 'id:from) (syntax-e (m 'id:to)) at-phase ie)
          spec]
         [(struct)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(struct id:struct (id:field ...))))
          (parse-struct! (m 'id:struct) (m 'id:field) at-phase ie)
          spec]
         [(all-from)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(all-from mod-path)))
          (parse-all-from (m 'mod-path) null at-phase ie)
          spec]
         [(all-from-except)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(all-from-except mod-path id ...)))
          (parse-all-from (m 'mod-path) (m 'id) at-phase ie)
          spec]
         [(all-defined)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(all-defined)))
          (parse-all-from-module self spec null #f at-phase ie)
          spec]
         [(all-defined-except)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(all-defined-except id ...)))
          (parse-all-from-module self spec (m 'id) #f at-phase ie)
          spec]
         [(prefix-all-defined)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(prefix-all-defined id:prefix)))
          (parse-all-from-module self spec null (syntax-e (m 'id:prefix)) at-phase ie)
          spec]
         [(prefix-all-defined-except)
          (check-nested 'phaseless)
          (define m (parse-syntax spec '(prefix-all-defined-except id:prefix id ...)))
          (parse-all-from-module self spec (m 'id) (syntax-e (m 'id:prefix)) at-phase ie)
          spec]
         [(expand)
          (void (parse-syntax spec '(expand (id . datum))))
          (define m (parse-syntax spec '(expand form)))
          (define exp-spec (expand (m 'form) ctx))
          (unless (and (pair? (syntax-e exp-spec))
                       (identifier? (car (syntax-e exp-spec)))
                       (eq? 'begin (core-form-sym (car (syntax-e exp-spec)))))
            (error "expansion of `provide` spec does not start `begin`:" spec))
          (define e-m (parse-syntax exp-spec '(begin spec ...)))
          (loop (e-m 'spec)
                at-phase
                protected?
                layer)]
         [else
          (cond
           [(identifier? spec)
            (parse-identifier! spec (syntax-e spec) phase ie)
            spec]
           [else
            (error "bad provide spec:" spec)])])))))

;; ----------------------------------------

(define (parse-identifier! spec sym at-phase ie)
  (define b (resolve spec at-phase))
  (unless b
    (error "provided identifier is not defined or imported:" spec))
  (register-export! ie sym at-phase b spec))

(define (parse-struct! id:struct fields at-phase ie)
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
    (parse-identifier! id (syntax-e id) at-phase ie))
  (for ([field (in-list fields)])
    (define get-id (mk2 "~a-~a" field))
    (define set-id (mk2 "set-~a-~a!" field))
    (parse-identifier! get-id (syntax-e get-id) at-phase ie)
    (parse-identifier! set-id (syntax-e set-id) at-phase ie)))
  
(define (parse-all-from mod-path-stx except-ids at-phase ie)
  (define mod-path (syntax->datum mod-path-stx))
  (unless (module-path? mod-path)
    (error "not a module path:" mod-path-stx))
  (parse-all-from-module (resolve-module-path mod-path) #f except-ids #f at-phase ie))
  
(define (parse-all-from-module mod-path matching-stx except-ids prefix-sym at-phase ie)
  (define ids+phases (register-extract-module-ids ie mod-path at-phase))
  (unless ids+phases
    (error "no imports from module path:" mod-path "at phase:" at-phase))
  
  (define (add-prefix sym)
    (if prefix-sym
        (string->symbol (format "~a~a" prefix-sym sym))
        sym))

  (define found (make-hasheq))
  
  ;; Register all except excluded bindings:
  (for ([id+phase (in-list ids+phases)])
    (define id (car id+phase))
    (define phase (cdr id+phase))
    (unless (or (and matching-stx
                     ;; For `(all-defined-out)`, binding context must match:
                     (not (free-identifier=? id
                                             (datum->syntax matching-stx (syntax-e id))
                                             phase)))
                (for/or ([except-id (in-list except-ids)])
                  (and (free-identifier=? id except-id phase)
                       (hash-set! found except-id #t))))
      (register-export! ie (add-prefix (syntax-e id)) phase (resolve id phase) id)))
  
  ;; Check that all exclusions matched something to exclude:
  (unless (= (hash-count found) (length except-ids))
    (for ([except-id (in-list except-ids)])
      (unless (or (hash-ref found except-id #f)
                  (for/or ([id+phase (in-list ids+phases)])
                    (define id (car id+phase))
                    (define phase (cdr id+phase))
                    (free-identifier=? id except-id phase)))
        (error (if matching-stx
                   "excluded identifier was not defined in the module:"
                   "excluded identifier was not required from the module:")
               except-id)))))

;; ----------------------------------------

(struct import-export (ids-to-binding  ; phase -> sym -> list of (cons id binding)
                       module-to-ids   ; resolved-module-name-> import-phase -> list of (cons id phase)
                       provided-syms)) ; phase -> sym -> module-binding

(define (make-import-export-registry)
  (import-export (make-hasheqv)
                 (make-hash)
                 (make-hasheqv)))

(define (register-defined-or-imported-id! ie id phase binding)
  (unless (equal? phase (phase+ (module-binding-nominal-phase binding)
                                (module-binding-nominal-import-phase binding)))
    (error "internal error: binding phase does not match nominal info"))

  (hash-update! (import-export-ids-to-binding ie)
                phase
                (lambda (at-phase)
                  (hash-update at-phase
                               (syntax-e id)
                               (lambda (l) (cons (cons id binding) l))
                               null))
                #hasheq())
  
  (hash-update! (import-export-module-to-ids ie)
                (module-binding-nominal-module binding)
                (lambda (at-mod)
                  (hash-update at-mod
                               (module-binding-nominal-import-phase binding)
                               (lambda (l) (cons (cons id phase) l))
                               null))
                #hasheqv()))

(define (register-export! ie sym phase binding id)
  (hash-update! (import-export-provided-syms ie)
                phase
                (lambda (at-phase)
                  (define b (hash-ref at-phase sym #f))
                  (cond
                   [(not b)
                    (hash-set at-phase sym b)]
                   [(and (equal? (module-binding-module b) (module-binding-module binding))
                         (eqv? (module-binding-phase b) (module-binding-phase binding))
                         (eq? (module-binding-sym b) (module-binding-sym binding)))
                    ;; If `binding` has different nomina info (i.e., same binding
                    ;; imported from different syntactic sources), we keep only
                    ;; the first once.
                    at-phase]
                   [else
                    (error "identifier already provided as a different binding:" id)]))
                #hasheq()))

(define (register-extract-module-ids ie mod-path phase)
  (define at-mod (hash-ref (import-export-module-to-ids ie) mod-path #f))
  (and at-mod
       (hash-ref at-mod phase null)))
