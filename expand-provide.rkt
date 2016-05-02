#lang racket/base
(require racket/set
         "stx.rkt"
         "scope.rkt"
         "pattern.rkt"
         "binding.rkt"
         "require.rkt"
         "module-path.rkt")

(provide parse-and-expand-provides
         
         make-import-export-registry
         register-defined-or-imported-id!)

(define layers '(raw phaseless id))

(define (parse-and-expand-provides specs
                                   import-export
                                   phase ctx
                                   expand rebuild)
  (let loop ([specs specs]
             [at-phase 0]
             [protected? #f]
             [layer 'raw]
             [wrap-expanded values])
    (define exports+expandeds
      (for/list ([spec (in-list specs)])
        (define (check-nested want-layer)
          (unless (member want-layer (member layer layers))
            (error "invalid nesting:" spec)))
        (define fm (and (pair? (syntax-e spec))
                        (identifier? (car (syntax-e spec)))
                        (syntax-e (car (syntax-e spec)))))
        (define-values (exports expanded)
          (case fm
            [(for-meta)
             (check-nested 'raw/no-just-meta)
             (define m (parse-syntax spec '(for-meta phase-level spec ...)))
             (define p (syntax-e (m 'phase-level)))
             (unless (phase? p)
               (error "bad phase:" spec))
             (loop (m 'spec)
                   p
                   protected?
                   'phaseless
                   (lambda (spec) 
                     (rebuild
                      spec
                      `(,(m 'for-meta) ,(m 'phase-level) ,@spec))))]
            [(for-syntax)
             (check-nested 'raw/no-just-meta)
             (define m (parse-syntax spec '(for-syntax spec ...)))
             (loop (m 'spec)
                   1
                   protected?
                   'phaseless
                   (lambda (spec) (rebuild spec `(,(m 'for-syntax) ,@spec))))]
            [(for-label)
             (check-nested 'raw/no-just-meta)
             (define m (parse-syntax spec '(for-label spec ...)))
             (loop (m 'spec)
                   #f
                   protected?
                   'phaseless
                   (lambda (spec) (rebuild spec `(,(m 'for-label) ,@spec))))]
            [(protect)
             (check-nested 'phaseless)
             (when protected?
               (error "invalid nesting:" spec))
             (define m (parse-syntax spec '(protect spec ...)))
             (loop (m 'spec)
                   at-phase
                   #t
                   layer
                   (lambda (spec) (rebuild spec `(,(m 'protect) ,@spec))))]
            [(rename)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(rename id:from id:to)))
             (values (list (cons (m 'id:from)
                                 (syntax-e (m 'id:to))))
                     (wrap-expanded spec))]
            [(struct)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(struct id:struct (id:field ...))))
             (define id:struct (m 'id:struct))
             (define (mk fmt)
               (define sym (string->symbol (format fmt (syntax-e id:struct))))
               (cons (datum->syntax id:struct sym id:struct)
                     sym))
             (define (mk2 fmt field-id)
               (define sym (string->symbol (format fmt
                                                   (syntax-e id:struct)
                                                   (syntax-e field-id))))
               (cons (datum->syntax id:struct sym id:struct)
                     sym))
             (values (list* (mk "~a")
                            (mk "make-~a")
                            (mk "struct:~a")
                            (mk "~a?")
                            (apply
                             append
                             (for/list ([field (in-list (m 'id:field))])
                               (list (mk2 "~a-~a" field)
                                     (mk2 "set-~a-~a!" field)))))
                     (wrap-expanded spec))]
            [(all-from)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(all-from mod-path)))
             (error "all-from")]
            [(all-from-except)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(all-from-except mod-path id ...)))
             (error "all-from-except")]
            [(all-defined)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(all-defined)))
             (error "all-defined")]
            [(all-defined-except)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(all-defined-except id ...)))
             (error "all-defined-except")]
            [(prefix-all-defined)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(prefix-all-defined id:prefix)))
             (error "all-defined")]
            [(prefix-all-defined-except)
             (check-nested 'phaseless)
             (define m (parse-syntax spec '(prefix-all-defined-except id:prefix id ...)))
             (error "all-defined-except")]
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
                   layer
                   wrap-expanded)]
            [else
             (cond
              [(identifier? spec)
               (define b (resolve spec at-phase))
               (unless b
                 (error "provided identifier is not defined or imported:" spec))
               (define sym (syntax-e spec))
               (register-export! import-export sym phase b spec)
               (values (list spec)
                       spec)]
              [else
               (error "bad provide spec:" spec)])]))
        (cons exports expanded)))
    (apply append (map car exports+expandeds))))

;; ----------------------------------------


;; ----------------------------------------

(struct import-export (ids-to-binding  ; phase -> sym -> list of (cons id binding)
                       module-to-ids   ; resolved-module-name-> phase -> list of id
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
                               phase
                               (lambda (l) (cons id l))
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
