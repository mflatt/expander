#lang racket/base
(require racket/set
         "syntax.rkt"
         "phase.rkt"
         "scope.rkt"
         "binding.rkt"
         "namespace.rkt"
         "match.rkt"
         "require+provide.rkt"
         "module-path.rkt")

(provide parse-and-perform-requires!
         perform-initial-require!)

(struct adjust-only (syms))
(struct adjust-prefix (sym))
(struct adjust-all-except (prefix-sym syms))
(struct adjust-rename (to-id from-sym))

(define layers '(raw raw/no-just-meta phaseless path))

(define (parse-and-perform-requires! reqs self m-ns phase-shift
                                     requires+provides
                                     #:run? [run? #f])
  (let loop ([reqs reqs]
             [top-req #f]
             [phase-shift phase-shift]
             [just-meta 'all]
             [adjust #f]
             [layer 'raw])
    (for ([req (in-list reqs)])
      (define (check-nested want-layer)
        (unless (member want-layer (member layer layers))
          (error "invalid nesting:" req)))
      (define fm (and (pair? (syntax-e req))
                      (identifier? (car (syntax-e req)))
                      (syntax-e (car (syntax-e req)))))
      (case fm
        [(for-meta)
         (check-nested 'raw/no-just-meta)
         (define m (match-syntax req '(for-meta phase-level spec ...)))
         (define p (syntax-e (m 'phase-level)))
         (unless (phase? p)
           (error "bad phase:" req))
         (loop (m 'spec) 
               (or top-req req)
               (phase+ phase-shift p)
               just-meta
               adjust
               'phaseless)]
        [(for-syntax)
         (check-nested 'raw/no-just-meta)
         (define m (match-syntax req '(for-syntax spec ...)))
         (loop (m 'spec)
               (or top-req req)
               (phase+ phase-shift 1)
               just-meta
               adjust
               'phaseless)]
        [(for-template)
         (check-nested 'raw/no-just-meta)
         (define m (match-syntax req '(for-template spec ...)))
         (loop (m 'spec)
               (or top-req req)
               (phase+ phase-shift -1)
               just-meta
               adjust
               'phaseless)]
        [(for-label)
         (check-nested 'raw/no-just-meta)
         (define m (match-syntax req '(for-label spec ...)))
         (loop (m 'spec)
               (or top-req req)
               (phase+ phase-shift #f)
               just-meta
               adjust
               'phaseless)]
        [(just-meta)
         (check-nested 'raw)
         (define m (match-syntax req '(just-meta phase-level spec ...)))
         (define p (syntax-e (m 'phase-level)))
         (unless (phase? p)
           (error "bad phase:" req))
         (loop (m 'spec)
               (or top-req req)
               phase-shift
               just-meta
               adjust
               'raw/no-just-meta)]
        [(only)
         (check-nested 'phaseless)
         (define m (match-syntax req '(only spec id ...)))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-only (ids->sym-set (m 'id)))
               'path)]
        [(prefix)
         (check-nested 'phaseless)
         (define m (match-syntax req '(prefix id:prefix spec)))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-prefix (syntax-e (m 'id:prefix)))
               'path)]
        [(all-except)
         (check-nested 'phaseless)
         (define m (match-syntax req '(all-except spec id ...)))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-all-except '|| (ids->sym-set (m 'id)))
               'path)]
        [(prefix-all-except)
         (check-nested 'phaseless)
         (define m (match-syntax req '(prefix-all-except id:prefix spec id ...)))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-all-except (syntax-e (m 'id:prefix)) (ids->sym-set (m 'id)))
               'path)]
        [(rename)
         (check-nested 'phaseless)
         (define m (match-syntax req '(rename spec id:to id:from)))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-rename (m 'id:to) (syntax-e (m 'id:from)))
               'path)]
        [else
         (define mp (syntax->datum req))
         (unless (module-path? mp)
           (error "bad require spec:" req))
         (perform-require! mp self
                           (or req top-req) m-ns phase-shift just-meta adjust
                           requires+provides
                           #:run? run?)]))))

(define (ids->sym-set ids)
  (for/set ([id (in-list ids)])
    (syntax-e id)))

;; ----------------------------------------

(define (perform-initial-require! mod-path self
                                  in-stx m-ns
                                  requires+provides)
  (perform-require! mod-path self
                    in-stx m-ns 0 'all #f
                    requires+provides
                    #:can-shadow? #t))

;; ----------------------------------------

(define (perform-require! mod-path self
                          in-stx m-ns phase-shift just-meta adjust
                          requires+provides
                          #:run? [run? #f]
                          #:can-shadow? [can-shadow? #f])
  (define mpi (module-path-index-join mod-path self))
  (define module-name (module-path-index-resolve mpi))
  (define bind-in-stx (if (adjust-rename? adjust)
                          (adjust-rename-to-id adjust)
                          in-stx))
  (define done-syms (make-hash))
  (add-required-module! requires+provides mpi phase-shift)
  (bind-all-provides!
   bind-in-stx phase-shift m-ns module-name mpi
   #:filter (lambda (binding)
              (define sym (module-binding-nominal-sym binding))
              (define provide-phase (module-binding-nominal-phase binding))
              (define adjusted-sym
                (cond
                 [(and (not (eq? just-meta 'all))
                       (not (equal? provide-phase just-meta)))
                  #f]
                 [(not adjust) sym]
                 [(adjust-only? adjust)
                  (and (set-member? (adjust-only-syms adjust) sym)
                       (hash-set! done-syms sym #t)
                       sym)]
                 [(adjust-prefix? adjust)
                  (string->symbol
                   (format "~a~a" (adjust-prefix-sym adjust) sym))]
                 [(adjust-all-except? adjust)
                  (and (not (and (set-member? (adjust-all-except-syms adjust) sym)
                                 (hash-set! done-syms sym #t)))
                       (string->symbol
                        (format "~a~a" (adjust-all-except-prefix-sym adjust) sym)))]
                 [(adjust-rename? adjust)
                  (and (eq? sym (adjust-rename-from-sym adjust))
                       (hash-set! done-syms sym #t)
                       (adjust-rename-to-id adjust))]))
              (when adjusted-sym
                (define s (datum->syntax bind-in-stx adjusted-sym))
                (define bind-phase (phase+ phase-shift provide-phase))
                (check-not-required-or-defined requires+provides
                                               s bind-phase)
                (add-defined-or-required-id! requires+provides
                                             s bind-phase binding
                                             #:can-shadow? can-shadow?))
              adjusted-sym))
  (namespace-module-visit! m-ns module-name phase-shift)
  (when run?
    (namespace-module-instantiate! m-ns module-name phase-shift))
  ;; check that we covered all expected ids:
  (define need-syms (cond
                    [(adjust-only? adjust)
                     (adjust-only-syms adjust)]
                    [(adjust-all-except? adjust)
                     (adjust-all-except-syms adjust)]
                    [(adjust-rename? adjust)
                     (set (adjust-rename-from-sym adjust))]
                    [else #f]))
  (when (and need-syms
             (not (= (set-count need-syms) (hash-count done-syms))))
    (for ([sym (in-set need-syms)])
      (unless (hash-ref done-syms sym #f)
        (error "not in nested spec:" sym)))))

;; ----------------------------------------

(define (bind-all-provides! in-stx phase-shift ns module-name mpi
                            #:filter filter)
  (define m (namespace->module ns module-name))
  (unless m
    (error "module not declared:" module-name))
  (define self (module-self-name m))
  (for ([(provide-phase-level provides) (in-hash (module-provides m))])
    (define phase (phase+ phase-shift provide-phase-level))
    (for ([(sym binding) (in-hash provides)])
      (define from-mod (module-binding-module binding))
      (define b (struct-copy module-binding binding
                             [module (if (eq? from-mod self)
                                         mpi
                                         from-mod)]
                             [nominal-module mpi]
                             [nominal-phase provide-phase-level]
                             [nominal-sym sym]
                             [nominal-require-phase phase-shift]))
      (let-values ([(sym) (filter b)])
        (when sym
          (add-binding! (datum->syntax in-stx sym) b phase))))))
