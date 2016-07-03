#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../common/contract.rkt"
         "module.rkt"
         "../namespace/provided.rkt"
         "../namespace/provide-for-api.rkt")

(provide compiled-expression?

         compiled-module-expression?
         module-compiled-name
         module-compiled-submodules
         module-compiled-language-info
         module-compiled-imports
         module-compiled-exports
         module-compiled-indirect-exports
         module-compiled-cross-phase-persistent?)

;; The representation of a module with its submodules is designed to
;; make reading an individual submodule (with its submodule path
;; intact) fast and convenient --- but it makes adjusting the name
;; inconvenient, because each linklet bundle for a module encodes its
;; full submodule path. The extra layer of `compiled-in-memory`
;; support for sharing and fast compile-then-eval cycles is another
;; layer of inconvenience.

(define (compiled-expression? c)
  (or (compiled-in-memory? c)
      (linklet-directory? c)))

(define (compiled->linklet-directory c)
  (if (compiled-in-memory? c)
      (compiled-in-memory-linklet-directory c)
      c))

(define (compiled-module-expression? c)
  (define ld (compiled->linklet-directory c))
  (and (linklet-directory? ld)
       (let ([b (hash-ref (linklet-directory->hash ld) #f #f)])
         (and b (hash-ref (linklet-bundle->hash b) 'decl #f)))
       #t))

(define module-compiled-name
  (case-lambda
    [(c)
     (check 'module-compiled-name compiled-module-expression? c)
     (hash-ref (linklet-bundle->hash
                (hash-ref (linklet-directory->hash (compiled->linklet-directory c)) #f))
               'name)]
    [(c name)
     (check 'module-compiled-name compiled-module-expression? c)
     (unless (or (symbol? name)
                 (and (pair? name)
                      (list? name)
                      (andmap symbol? name)))
       (raise-argument-error 'module-compiled-name
                             "(or/c symbol? (cons/c symbol? (non-empty-listof symbol?)))"
                             name))
     (define prefix (if (symbol? name)
                        null
                        (reverse (cdr (reverse name)))))
     (change-module-name c name prefix)]))

(define module-compiled-submodules
  (case-lambda
    [(c non-star?)
     (check 'module-compiled-submodules compiled-module-expression? c)
     (cond
      [(compiled-in-memory? c)
       ;; We have a convenient `compiled-in-memory` structure
       (if non-star?
           (compiled-in-memory-pre-compiled-in-memorys c)
           (compiled-in-memory-post-compiled-in-memorys c))]
      [else 
       ;; We have a raw linklet directory, which is designed more for
       ;; loading code than easy manipulation...
       (define ht (linklet-directory->hash c))
       (define bh (linklet-bundle->hash (hash-ref ht #f)))
       (define names (hash-ref bh (if non-star? 'pre 'post) null))
       (for/list ([name (in-list names)])
         (hash-ref ht name))])]
    [(c non-star? submods)
     (check 'module-compiled-submodules compiled-module-expression? c)
     (unless (and (list? submods)
                  (andmap compiled-module-expression? submods))
       (raise-argument-error 'module-compiled-submodules "(listof compiled-module-expression?)" submods))
     (cond
      [(and (compiled-in-memory? c)
            (andmap compiled-in-memory? submods))
       ;; All compiled-in-memory structures, so preserve them
       (define pre-compiled-in-memorys (if non-star?
                                           submods
                                           (compiled-in-memory-pre-compiled-in-memorys c)))
       (define post-compiled-in-memorys (if non-star?
                                            (compiled-in-memory-post-compiled-in-memorys c)
                                            submods))
       (fixup-submodule-names
        (struct-copy compiled-in-memory c
                     [pre-compiled-in-memorys pre-compiled-in-memorys]
                     [post-compiled-in-memorys post-compiled-in-memorys]
                     [linklet-directory (rebuild-linklet-directory
                                         (reset-submodule-names
                                          (hash-ref (linklet-directory->hash (compiled->linklet-directory c)) #f)
                                          non-star?
                                          submods)
                                         (append pre-compiled-in-memorys
                                                 post-compiled-in-memorys))]))]
      [else
       ;; Not all compiled-in-memory structures, so forget whatever ones we have
       (fixup-submodule-names
        (rebuild-linklet-directory
         (reset-submodule-names
          (hash-ref (linklet-directory->hash (compiled->linklet-directory c)) #f)
          non-star?
          submods)
         (map compiled->linklet-directory
              (append (if non-star? submods (module-compiled-submodules c #t))
                      (if non-star? (module-compiled-submodules c #f) submods)))))])]))

(define (module-compiled-language-info c)
  (check 'module-compiled-language-info compiled-module-expression? c)  
  (define inst (compiled-module->declaration-instance c))
  (instance-variable-value inst 'language-info))

(define (module-compiled-imports c)
  (check 'module-compiled-imports compiled-module-expression? c)
  (define inst (compiled-module->declaration-instance c))
  (instance-variable-value inst 'requires))

(define (module-compiled-exports c)
  (check 'module-compiled-imports compiled-module-expression? c)
  (define inst (compiled-module->declaration-instance c))
  (provides->api-provides (instance-variable-value inst 'provides)))

(define (module-compiled-indirect-exports c)
  (check 'module-compiled-indirect-imports compiled-module-expression? c)
  (define-values (h inst) (compiled-module->h+declaration-instance c))
  (define min-phase (instance-variable-value inst 'min-phase))
  (define max-phase (instance-variable-value inst 'max-phase))
  (variables->api-nonprovides (instance-variable-value inst 'provides)
                              (for/hash ([phase-level (in-range min-phase (add1 max-phase))])
                                (define linklet (hash-ref h phase-level #f))
                                (values phase-level
                                        (if linklet
                                            (linklet-export-variables linklet)
                                            null)))))

(define (module-compiled-cross-phase-persistent? c)
  (check 'module-compiled-cross-phase-persistent?  compiled-module-expression? c)
  (define inst (compiled-module->declaration-instance c))
  (instance-variable-value inst 'cross-phase-persistent?))

;; ----------------------------------------

(define (module-compiled-immediate-name c)
  (car (reverse (module-compiled-name c))))

(define (change-module-name c name prefix)
  (define full-name (if (null? prefix) name (append prefix (list name))))
  (define next-prefix (if (null? prefix) (list name) full-name))
  (define (recur sub-c name)
    (if (equal? (module-compiled-name sub-c) (append next-prefix (list name)))
        sub-c
        (change-module-name sub-c name next-prefix)))
  (cond
   [(compiled-in-memory? c)
    (define (change-submodule-name sub-c)
      (recur sub-c (module-compiled-immediate-name sub-c)))
    (define pre-compiled-in-memorys (map change-submodule-name
                                         (compiled-in-memory-pre-compiled-in-memorys c)))
    (define post-compiled-in-memorys (map change-submodule-name
                                          (compiled-in-memory-post-compiled-in-memorys c)))
    (struct-copy compiled-in-memory c
                 [pre-compiled-in-memorys pre-compiled-in-memorys]
                 [post-compiled-in-memorys post-compiled-in-memorys]
                 [linklet-directory (rebuild-linklet-directory
                                     (update-one-name
                                      (hash-ref (linklet-directory->hash (compiled->linklet-directory c)) #f)
                                      full-name)
                                     (append pre-compiled-in-memorys
                                             post-compiled-in-memorys))])]
   [else
    (hash->linklet-directory
     (for/hasheq ([(key val) (in-hash (linklet-directory->hash c))])
       (values key
               (if (not key)
                   (update-one-name val full-name)
                   (recur val key)))))]))

(define (update-one-name lb name)
  (hash->linklet-bundle (hash-set (linklet-bundle->hash lb) 'name name)))

(define (fixup-submodule-names c)
  ;; Although this looks like a no-op, it forces a reset on submodule
  ;; names, except where the names already match (short-circuited in
  ;; `change-module-name`).
  (module-compiled-name c (module-compiled-name c)))

(define (rebuild-linklet-directory main submods)
  (hash->linklet-directory
   (hash-set (for/fold ([ht #hasheq()]) ([submod (in-list submods)])
               (define name (module-compiled-immediate-name submod))
               (cond
                [(hash-ref ht name #f)
                 (raise-arguments-error 'module-compiled-submodules
                                        "change would result in duplicate submodule name"
                                        "name" name)]
                [else
                 (hash-set ht name submod)]))
             #f
             main)))

(define (reset-submodule-names b pre? submods)
  (hash->linklet-bundle
   (hash-set (linklet-bundle->hash b)
             (if pre? 'pre 'post)
             (map module-compiled-immediate-name submods))))
