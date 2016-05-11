#lang racket/base
(require racket/list)

(provide resolved-module-path?
         make-resolved-module-path
         resolved-module-path-name
         
         module-path-index?
         module-path-index-resolve
         module-path-index-join
         module-path-index-split
         module-path-index-submodule
         make-self-module-path-index
         
         resolve-module-path
         current-module-name-resolver
         build-module-name)

;; ----------------------------------------

;; ----------------------------------------

(struct resolved-module-path (name)
        #:property prop:custom-write
        (lambda (r port mode)
          (write-string "#<resolved-module-path:" port)
          (fprintf port " ~.s" (resolved-module-path-name r))
          (write-string ">" port)))

;; FIXME: make weak
(define resolved-module-paths (make-hash))

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
  (or (hash-ref resolved-module-paths p #f)
      (let ([r (resolved-module-path p)])
        (hash-set! resolved-module-paths p r)
        r)))

;; ----------------------------------------

(struct module-path-index (path base [resolved #:mutable])
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
          (when (module-path-index-path r)
            (fprintf port ":~.s" (module-path-index-path r)))
          (when (module-path-index-resolved r)
            (fprintf port "=~.s" (module-path-index-resolved r)))
          (write-string ">" port)))

(define (module-path-index-resolve mpi)
  (unless (module-path-index? mpi)
    (raise-argument-error 'module-path-index-resolve "module-path-index?" mpi))
  (or (module-path-index-resolved mpi)
      (let ([mod-name ((current-module-name-resolver)
                       (module-path-index-path mpi)
                       (module-path-index-resolve/maybe
                        (module-path-index-base mpi))
                       #f
                       #f)])
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
  (when submod (error "not yet implemented"))
  (module-path-index mod-path base #f))

(define (module-path-index-resolve/maybe base)
  (if (module-path-index? base)
      (module-path-index-resolve base)
      base))

(define (module-path-index-split mpi)
  (unless (module-path-index? mpi)
    (raise-argument-error 'module-path-index-split "module-path-index?" mpi))
  (values (module-path-index-path mpi)
          (module-path-index-base mpi)))

(define (module-path-index-submodule mpi)
  (error "not yet implemented"))

(define (make-self-module-path-index r)
  (module-path-index #f #f r))

;; ----------------------------------------

(define (resolve-module-path mod-path base)
  ((current-module-name-resolver) mod-path base #f #t))

(define current-module-name-resolver
  (make-parameter
   (lambda (p enclosing source-stx-stx load?)
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
              "not a supported module path: ~v" p)]))))

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
      [else (drop-right enclosing-module-name 1)])]
    [else (append enclosing-module-name (list name))])))
