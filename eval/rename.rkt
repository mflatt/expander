#lang racket/base

(define (change-module-name c name prefix)
  (define full-name (if (null? prefix) name (append prefix (list name))))
  (define next-prefix (if (null? prefix) (list name) full-name))
  (cond
   [(compiled-in-memory? c)
    (define ld-to-name (for/hasheq ([(key val) (in-hash (linklet-directory->hash
                                                         (compiled-in-memory-linklet-directory c)))])
                         (values val key)))
    (define (change-submodule-name sub-c)
      (change-module-name sub-c (hash-ref ld-to-name sub-c) next-prefix))
    (define pre-compiled-in-memorys (map change-submodule-name
                                         (compiled-in-memory-pre-compiled-in-memorys)))
    (define post-compiled-in-memorys (map change-submodule-name
                                          (compiled-in-memory-post-compiled-in-memorys)))
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
                   (change-module-name val key next-prefix)))))]))
                   
  
