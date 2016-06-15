#lang racket/base
(require "../compile/serialize-property.rkt"
         "contract.rkt")

(provide resolved-module-path?
         make-resolved-module-path
         resolved-module-path-name
         resolved-module-path-root-name
         resolved-module-path->module-path
         format-resolved-module-path-name
         
         module-path-index?
         module-path-index-resolve
         module-path-index-join
         module-path-index-split
         module-path-index-submodule
         make-self-module-path-index
         make-generic-self-module-path-index
         imitate-generic-module-path-index!
         module-path-index-shift
         module-path-index-resolved ; returns #f if not yet resolved

         top-level-module-path-index
         top-level-module-path-index?

         resolve-module-path
         current-module-name-resolver
         build-module-name
         
         current-module-declare-name
         substitute-module-declare-name
         
         deserialize-module-path-index)

;; ----------------------------------------

(struct resolved-module-path (name)
        #:property prop:custom-write
        (lambda (r port mode)
          (write-string "#<resolved-module-path:" port)
          (fprintf port "~a" (format-resolved-module-path-name (resolved-module-path-name r)))
          (write-string ">" port))
        #:property prop:serialize
        (lambda (r ser state)
          `(deserialize-resolved-module-path
            ,(ser (resolved-module-path-name r)))))

(define (deserialize-resolved-module-path n)
  (make-resolved-module-path n))

(define (format-resolved-module-path-name p)
  (cond
   [(path? p) (path->string p)]
   [(symbol? p) (format "'~s" p)]
   [else (format "(submod ~a~a)"
                 (format-resolved-module-path-name (car p))
                 (apply string-append (for/list ([i (cdr p)])
                                        (format " ~s" i))))]))

(define (resolved-module-path-root-name r)
  (define name (resolved-module-path-name r))
  (if (pair? name)
      (car name)
      name))

(define resolved-module-paths (make-weak-hash))

(define (make-resolved-module-path p)
  (unless (or (symbol? p)
              (and (path? p) (complete-path? p))
              (and (pair? p)
                   (pair? (cdr p))
                   (list? p)
                   (or (symbol? (car p))
                       (and (path? (car p)) (complete-path? (car p))))
                   (for/and ([s (in-list (cdr p))])
                     (symbol? s))))
    (raise-argument-error 'make-resolved-module-path
                          (string-append
                           "(or/c symbol?\n"
                           "      (and/c path? complete-path?)\n"
                           "      (cons/c (or/c symbol?\n"
                           "                    (and/c path? complete-path?))\n"
                           "              (non-empty-listof symbol?)))")
                          p))
  ;; FIXME: make atomic
  (or (let ([e (hash-ref resolved-module-paths p #f)])
        (and e (ephemeron-value e)))
      (let ([r (resolved-module-path p)])
        (hash-set! resolved-module-paths p (make-ephemeron p r))
        r)))

(define (resolved-module-path->module-path r)
  (define name (resolved-module-path-name r))
  (define root-name (if (pair? name) (car name) name))
  (define root-mod-path (if (path? root-name)
                            root-name
                            `(quote ,root-name)))
  (if (pair? name)
      `(submod ,root-mod-path ,@(cdr name))
      root-mod-path))

;; ----------------------------------------

(struct module-path-index (path base [resolved #:mutable] shift-cache)
        #:property prop:equal+hash
        (list (lambda (a b eql?)
                (and (eql? (module-path-index-path a)
                           (module-path-index-path b))
                     (eql? (module-path-index-base a)
                           (module-path-index-base b))))
              (lambda (a hash-code)
                (and (+ (hash-code (module-path-index-path a))
                        (hash-code (module-path-index-base a)))))
              (lambda (a hash-code)
                (and (+ (hash-code (module-path-index-path a))
                        (hash-code (module-path-index-base a))))))
        #:property prop:custom-write
        (lambda (r port mode)
          (write-string "#<module-path-index" port)
          (fprintf port "[~a]" (eq-hash-code r))
          (cond
           [(top-level-module-path-index? r)
            (fprintf port ":top-level")]
           [(module-path-index-path r)
            (define l (let loop ([r r])
                        (cond
                         [(not r) null]
                         [(module-path-index-path r)
                          (cons (format "~.s" (module-path-index-path r))
                                (loop (module-path-index-base r)))]
                         [(module-path-index-resolved r)
                          (list
                           "+"
                           (format-resolved-module-path-name
                            (resolved-module-path-name
                             (module-path-index-resolved r))))]
                         [else null])))
            (fprintf port ":~.a" (apply string-append
                                        (car l)
                                        (for/list ([i (in-list (cdr l))])
                                          (format " ~a" i))))]
           [(module-path-index-resolved r)
            (fprintf port "=~a" (format-resolved-module-path-name
                                 (resolved-module-path-name
                                  (module-path-index-resolved r))))])
          (write-string ">" port)))

;; Serialization of a module path index is handled specially, because they
;; must be shared across phases of a module
(define deserialize-module-path-index
  (case-lambda
    [(path base) (module-path-index-join path base)]
    [(name) (make-self-module-path-index (make-resolved-module-path name))]
    [() top-level-module-path-index]))

(define (module-path-index-resolve mpi [load? #f])
  (check 'module-path-index-resolve module-path-index? mpi)
  (or (module-path-index-resolved mpi)
      (let ([mod-name ((current-module-name-resolver)
                       (module-path-index-path mpi)
                       (module-path-index-resolve/maybe
                        (module-path-index-base mpi)
                        load?)
                       #f
                       load?)])
        (unless (resolved-module-path? mod-name)
          (raise-arguments-error 'module-path-index-resolve
                                 "current module name resolver's result is not a resolved module path"
                                 "result" mod-name))
        (set-module-path-index-resolved! mpi mod-name)
        mod-name)))

(define (module-path-index-join mod-path base [submod #f])
  (unless (or (not mod-path)
              (module-path? mod-path))
    (raise-argument-error 'module-path-index-join "(or/c #f module-path?)" mod-path))
  (unless (or (not base)
              (resolved-module-path? base)
              (module-path-index? base))
    (raise-argument-error 'module-path-index-join "(or/c #f resolved-module-path? module-path-index?)" base))
  (unless (or (not submod)
              (and (pair? submod)
                   (list? submod)
                   (andmap symbol? submod)))
    (raise-argument-error 'module-path-index-join "(or/c #f (non-empty-listof symbol?))" submod))
  (when (and (not mod-path)
             base)
    (raise-arguments-error 'module-path-index-join
                           "cannot combine #f path with non-#f base"
                           "given base" base))
  (when (and submod mod-path)
    (raise-arguments-error 'module-path-index-join
                           "cannot combine #f submodule list with non-#f module path"
                           "given module path" mod-path
                           "given submodule list" submod))
  (cond
   [submod
    (make-self-module-path-index (make-resolved-module-path
                                  (cons generic-module-name submod)))]
   [else
    (define keep-base
      (cond
       [(path? mod-path) #f]
       [(and (pair? mod-path) (eq? 'quote (car mod-path))) #f]
       [(symbol? mod-path) #f]
       [else base]))
    (module-path-index mod-path keep-base #f (make-shift-cache))]))

(define (module-path-index-resolve/maybe base load?)
  (if (module-path-index? base)
      (module-path-index-resolve base load?)
      base))

(define (module-path-index-split mpi)
  (check 'module-path-index-split module-path-index? mpi)
  (values (module-path-index-path mpi)
          (module-path-index-base mpi)))

(define (module-path-index-submodule mpi)
  (check 'module-path-index-submodule module-path-index? mpi)
  (and (not (module-path-index-path mpi))
       (let ([r (module-path-index-resolved mpi)])
         (and r
              (let ([p (resolved-module-path-name r)])
                (and (pair? p)
                     (cdr p)))))))

(define make-self-module-path-index
  (case-lambda
    [(name) (module-path-index #f #f name (make-shift-cache))]
    [(name enclosing)
     (make-self-module-path-index (build-module-name name
                                                     (and enclosing
                                                          (module-path-index-resolve enclosing))))]))

;; A "generic" module path index is used by the exansion of `module`; every
;; expanded module (at the same submodule nesting and name) uses the same
;; generic module path, so that compilation can recognize references within
;; the module to itself, and so on
(define generic-self-mpis (make-weak-hash))
(define generic-module-name 'expanded)

;; Return a module path index that is the same for a given
;; submodule path in the given self module path index
(define (make-generic-self-module-path-index self)
  (define r (resolved-module-path-to-generic-resolved-module-path
             (module-path-index-resolved self)))
  (or (let ([e (hash-ref generic-self-mpis r #f)])
        (and e (ephemeron-value e)))
      (let ([mpi (module-path-index #f #f r (make-shift-cache))])
        (hash-set! generic-self-mpis r (make-ephemeron r mpi))
        mpi)))

(define (resolved-module-path-to-generic-resolved-module-path r)
  (define name (resolved-module-path-name r))
  (make-resolved-module-path
   (if (symbol? name)
       generic-module-name
       (cons generic-module-name (cdr name)))))

;; Mutate the resolved path in `mpi` to use the root module name of a
;; generic module path index, which means that future
;; `free-identifier=?` comparisons with the generic module path index
;; will succeed
(define (imitate-generic-module-path-index! mpi)
  (define r (module-path-index-resolved mpi))
  (when r
    (set-module-path-index-resolved! mpi
                                     (resolved-module-path-to-generic-resolved-module-path r))))

(define (module-path-index-shift mpi from-mpi to-mpi)
  (cond
   [(eq? mpi from-mpi) to-mpi]
   [else
    (define base (module-path-index-base mpi))
    (cond
     [(not base) mpi]
     [else
      (define shifted-base (module-path-index-shift base from-mpi to-mpi))
      (cond
       [(eq? shifted-base base) mpi]
       [(shift-cache-ref (module-path-index-shift-cache shifted-base) mpi)]
       [else
        (define shifted-mpi
          (module-path-index (module-path-index-path mpi)
                             shifted-base
                             #f
                             (make-shift-cache)))
        (shift-cache-set! (module-path-index-shift-cache shifted-base) mpi shifted-mpi)
        shifted-mpi])])]))

(define (make-shift-cache)
  (make-weak-hasheq))

(define (shift-cache-ref cache v)
  (hash-ref cache v #f))

(define (shift-cache-set! cache v r)
  (hash-set! cache v r))

;; A constant module path index to represent the top level
(define top-level-module-path-index
  (make-self-module-path-index
   (make-resolved-module-path 'top-level)))

(define (top-level-module-path-index? mpi)
  (eq? top-level-module-path-index mpi))

;; ----------------------------------------

(define (resolve-module-path mod-path base)
  ((current-module-name-resolver) mod-path base #f #t))

(define current-module-name-resolver
  (make-parameter
   (case-lambda
     [(name from-namespace)
      ;; No need to register
      (void)]
     [(p enclosing source-stx-stx load?)
      (unless (module-path? p)
        (raise-argument-error 'core-module-name-resolver "module-path?" p))
      (unless (or (not enclosing)
                  (resolved-module-path? enclosing))
        (raise-argument-error 'core-module-name-resolver "resolved-module-path?" enclosing))
      (cond
       [(and (list? p)
             (= (length p) 2)
             (eq? 'quote (car p))
             (symbol? (cadr p)))
        (make-resolved-module-path (cadr p))]
       [(and (list? p)
             (eq? 'submod (car p))
             (equal? ".." (cadr p)))
        (for/fold ([enclosing enclosing]) ([s (in-list (cdr p))])
          (build-module-name s enclosing #:original p))]
       [(and (list? p)
             (eq? 'submod (car p))
             (equal? "." (cadr p)))
        (for/fold ([enclosing enclosing]) ([s (in-list (cddr p))])
          (build-module-name s enclosing #:original p))]
       [(and (list? p)
             (eq? 'submod (car p)))
        (let ([base ((current-module-name-resolver) (cadr p) enclosing #f #f)])
          (for/fold ([enclosing base]) ([s (in-list (cddr p))])
            (build-module-name s enclosing #:original p)))]
       [else
        (error 'core-module-name-resolver
               "not a supported module path: ~v" p)])])))

;; Build a submodule name given an enclosing module name, if cany
(define (build-module-name name ; a symbol
                           enclosing ; #f => no enclosing module
                           #:original [orig-name name]) ; for error reporting
  (define enclosing-module-name (and enclosing
                                     (resolved-module-path-name enclosing)))
  (make-resolved-module-path
   (cond
    [(not enclosing-module-name) name]
    [(symbol? enclosing-module-name) (list enclosing-module-name name)]
    [(equal? name "..")
     (cond
      [(symbol? enclosing-module-name)
       (error "too many \"..\"s:" orig-name)]
      [(= 2 (length enclosing-module-name)) (car enclosing-module-name)]
      [else (reverse (cdr (reverse enclosing-module-name)))])]
    [else (append enclosing-module-name (list name))])))

;; ----------------------------------------

(define current-module-declare-name
  (make-parameter #f
                  (lambda (r)
                    (unless (or (not r)
                                (resolved-module-path? r))
                      (raise-argument-error 'current-module-declare-name
                                            "(or/c #f resolved-module-path?)"
                                            r))
                    r)))

(define (substitute-module-declare-name default-root-name default-name)
  (define current-name (current-module-declare-name))
  (define root-name (if current-name
                        (resolved-module-path-root-name current-name)
                        default-root-name))
  (make-resolved-module-path
   (if (pair? default-name)
       (cons root-name (cdr default-name))
       root-name)))
