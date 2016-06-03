#lang racket/base
(require "scope.rkt"
         "module-use.rkt"
         "module-path.rkt"
         "compile-context.rkt"
         "built-in-symbol.rkt"
         "compile-impl-id.rkt"
         "serialize.rkt")

(provide (struct-out header)
         make-header
         
         add-syntax-literal!
         generate-syntax-literals!
         syntax-literals-as-vector-getter
         empty-syntax-literals?
         
         local-key->symbol
         select-fresh

         register-required-variable-use!
         generate-links+imports)

(struct header (module-path-indexes        ; module-path-index -> linklet import position
                binding-sym-to-define-sym  ; sym -> sym; avoid conflicts with primitives
                [binding-syms-in-order #:mutable] ; list of sym
                require-var-to-import-sym  ; variable-use -> sym
                [require-vars-in-order #:mutable] ; list of variable-use
                define-and-import-syms     ; hash of sym, to select distinct symbols
                syntax-literals            ; box of list of syntax-literal
                [num-syntax-literals #:mutable]))

(struct variable-use (module-use sym)
        #:transparent) ; for hashing


(define (make-header mpis)
  (header mpis
          (make-hasheq)        ; binding-sym-to-define-sym
          null                 ; binding-syms-in-order
          (make-variable-uses) ; require-var-to-import-sym
          null                 ; require-vars-in-order
          (make-hasheq)        ; define-and-import-syms
          (box null)
          0))

(define (make-variable-uses)
  (make-hash))

(define (add-syntax-literal! header q)
  (define pos (header-num-syntax-literals header))
  (set-header-num-syntax-literals! header (add1 pos))
  (define b (header-syntax-literals header))
  (set-box! b (cons q (unbox b)))
  pos)

(define (empty-syntax-literals? syntax-literals)
  (null? (unbox syntax-literals)))

(define (generate-syntax-literals! syntax-literals-box mpis phase self)
  (define syntax-literals (unbox syntax-literals-box))
  (define len (length syntax-literals))
  (cond
   [(zero? len) null]
   [else
    `((define-values (,syntax-literals-id) (make-vector ,len #f))
      (define-values (,get-syntax-literal!-id)
        (lambda (pos)
          (begin
            (if (vector-ref ,deserialized-syntax-id ,phase)
                (void)
                (vector-set! ,deserialized-syntax-id
                             ,phase
                             ,(generate-deserialize (vector->immutable-vector
                                                     (list->vector (reverse syntax-literals)))
                                                    mpis)))
            (let-values ([(stx)
                          (syntax-module-path-index-shift
                           (syntax-shift-phase-level
                            (vector-ref (vector-ref ,deserialized-syntax-id ,phase) pos) ,phase-shift-id)
                           ,(add-module-path-index! mpis self)
                           ,self-id
                           ,bulk-binding-registry-id)])
              (begin
                (vector-set! ,syntax-literals-id pos stx)
                stx))))))]))

(define (syntax-literals-as-vector-getter syntax-literals-box)
  (define syntax-literals (unbox syntax-literals-box))
  (define vec (list->vector (reverse syntax-literals)))
  (lambda (phase-shift)
    (if (zero? phase-shift)
        vec
        (for/vector #:length (vector-length vec) ([s (in-vector vec)])
                    (syntax-shift-phase-level s phase-shift)))))

;; ----------------------------------------

;; Pick a symbol to represent a local binding, given the binding's key
(define (local-key->symbol key)
  ;; A local-binding key is already an distinct uninterned symbol
  ;; (with a deterministic label)
  key)

;; Select a symbol not yet used in the header or as a built-in name
(define (select-fresh sym header)
  (if (symbol-conflicts? sym header)
      (let loop ([pos 1])
        (define new-sym (string->symbol (format "~a/~a" pos sym)))
        (if (symbol-conflicts? new-sym header)
            (loop (add1 pos))
            new-sym))
      sym))

(define (symbol-conflicts? sym header)
  (or (built-in-symbol? sym)
      (hash-ref (header-define-and-import-syms header) sym #f)))

;; ----------------------------------------

(define (register-required-variable-use! header mpi phase sym)
  (define key (variable-use (module-use mpi phase) sym))
  (define variable-uses (header-require-var-to-import-sym header))
  (or (hash-ref variable-uses key #f)
      (let ([sym (select-fresh (variable-use-sym key) header)])
        (hash-set! variable-uses key sym)
        (set-header-require-vars-in-order! header
                                           (cons key
                                                 (header-require-vars-in-order header)))
        (hash-set! (header-define-and-import-syms header) sym #t)
        sym)))

;; Returns:
;;  link-names : a list of sym
;;  link-requires : a list of module path indexes
;;  imports : a list of S-expressions for imports; refers to `link-names`
;;  def-decls : a list of S-expressions for forward-reference declarations
(define (generate-links+imports header phase cctx)
  ;; Make a link symbol for each distinct module+phase:
  (define mod-use-to-link-sym
    (for/fold ([ht #hash()]) ([(vu) (in-list (header-require-vars-in-order header))])
      (define mu (variable-use-module-use vu))
      (if (or (hash-ref ht mu #f)
              (eq? (module-use-module mu)
                   (compile-context-self cctx)))
          ht
          (hash-set ht mu (string->symbol
                           (format "~a_~a_~a"
                                   (extract-name (module-use-module mu))
                                   (module-use-phase mu)
                                   (hash-count ht)))))))
  ;; List of distinct module+phases:
  (define link-mod-uses (hash-keys mod-use-to-link-sym))

  (values
   ;; Module-uses list:
   link-mod-uses
   ;; Imports, using the same order as module-uses list:
   (for/list ([mu (in-list link-mod-uses)])
     (cons (hash-ref mod-use-to-link-sym mu)
           (for/list ([vu (in-list (header-require-vars-in-order header))]
                      #:when (equal? mu (variable-use-module-use vu)))
             (define var-sym (hash-ref (header-require-var-to-import-sym header) vu))
             `[,(variable-use-sym vu) ,var-sym])))
   ;; Declarations (for non-module contexts)
   (for/list ([vu (in-list (header-require-vars-in-order header))]
              #:when (eq? (module-use-module (variable-use-module-use vu))
                          (compile-context-self cctx)))
     (define var-sym (hash-ref (header-require-var-to-import-sym header) vu))
     `(,var-sym ,(variable-use-sym vu)))))

;; Get a reasonably nice name from a module-path-index
(define (extract-name mpi)
  (define-values (p base) (module-path-index-split mpi))
  (cond
   [(symbol? p) p]
   [(path? p) (let-values ([(base name dir?) (split-path p)])
                (path-replace-extension name #""))]
   [(string? p) (path-replace-extension p #"")]
   [(and (pair? p) (eq? (car p) 'quote))
    (cadr p)]
   [(and (pair? p) (eq? (car p) 'file))
    (let-values ([(base name dir?) (split-path (cadr p))])
      (path-replace-extension name #""))]
   [(and (pair? p) (eq? (car p) 'lib))
    (path-replace-extension (cadr p) #"")]
   [else 'module]))

