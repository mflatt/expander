#lang racket/base
(require "../syntax/syntax.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/binding.rkt"
         "require+provide.rkt"
         "context.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt")

(provide parse-and-expand-provides!)

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
          (list
           (rebuild
            spec
            `(,(m 'for-syntax) ,@(loop (m 'spec)
                                       (phase+ 1 at-phase)
                                       protected?
                                       'phaseless))))]
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
          (parse-all-from (m 'mod-path) self null at-phase rp)
          (list spec)]
         [(all-from-except)
          (check-nested 'phaseless)
          (define m (match-syntax spec '(all-from-except mod-path id ...)))
          (parse-all-from (m 'mod-path) self (m 'id) at-phase rp)
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
                                                          [only-immediate? #t])))
          (unless (and (pair? (syntax-e exp-spec))
                       (identifier? (car (syntax-e exp-spec)))
                       (eq? 'begin (core-form-sym exp-spec at-phase)))
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
  (define b (resolve+shift spec at-phase))
  (unless b
    (error "provided identifier is not defined or required:" spec))
  (add-provide! rp sym at-phase b spec))

(define (parse-struct! id:struct fields at-phase rp)
  (define (mk fmt)
    (define sym (string->symbol (format fmt (syntax-e id:struct))))
    (datum->syntax id:struct sym id:struct))
  (define (mk2 fmt field-id)
    (define sym (string->symbol (format fmt
                                        (syntax-e id:struct)
                                        (syntax-e field-id))))
    (datum->syntax id:struct sym id:struct))
  (for ([fmt (list "~a"
                   "make-~a"
                   "struct:~a"
                   "~a?")])
    (define id (mk fmt))
    (parse-identifier! id (syntax-e id) at-phase rp))
  (for ([field (in-list fields)])
    (define get-id (mk2 "~a-~a" field))
    (define set-id (mk2 "set-~a-~a!" field))
    (parse-identifier! get-id (syntax-e get-id) at-phase rp)
    (parse-identifier! set-id (syntax-e set-id) at-phase rp)))
  
(define (parse-all-from mod-path-stx self except-ids at-phase rp)
  (define mod-path (syntax->datum mod-path-stx))
  (unless (module-path? mod-path)
    (error "not a module path:" mod-path-stx))
  (define mpi (module-path-index-join mod-path self))
  (parse-all-from-module mpi #f except-ids #f at-phase rp))
  
(define (parse-all-from-module mpi matching-stx except-ids prefix-sym at-phase rp)
  (define requireds (extract-module-requires rp mpi at-phase))
  (unless requireds
    (error "no requires from module path:" mpi "at phase:" at-phase))
  
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
                                             phase
                                             phase)))
                (for/or ([except-id (in-list except-ids)])
                  (and (free-identifier=? id except-id phase phase)
                       (hash-set! found except-id #t))))
      (add-provide! rp (add-prefix (syntax-e id)) phase (resolve+shift id phase) id)))
  
  ;; Check that all exclusions matched something to exclude:
  (unless (= (hash-count found) (length except-ids))
    (for ([except-id (in-list except-ids)])
      (unless (or (hash-ref found except-id #f)
                  (for/or ([i (in-list requireds)])
                    (define id (required-id i))
                    (define phase (required-phase i))
                    (free-identifier=? id except-id phase phase)))
        (error (if matching-stx
                   "excluded identifier was not defined in the module:"
                   "excluded identifier was not required from the module:")
               except-id)))))
