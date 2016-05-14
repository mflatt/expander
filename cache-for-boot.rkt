#lang racket/base
(require racket/serialize
         racket/file
         racket/fasl
         file/sha1)

(provide get-cached-compiled
         cache-compiled!
         cache-prime!)

(define original-compile (current-compile))
(define original-eval (current-eval))

(define (cache-dir->file cache-dir)
  (build-path cache-dir "cache.rktd"))

(define (get-cached-compiled cache-dir path [notify-success void])
  (define cache-file (cache-dir->file cache-dir))
  (define cache
    (if (file-exists? cache-file)
        (call-with-input-file* cache-file read)
        #hash()))
  (define cached-filename (hash-ref cache (path->string path) #f))
  (define cached-file (and cached-filename
                           (build-path cache-dir cached-filename)))
  (define (show v) (log-error "~.s" v) v)
  (and cached-file
       (file-exists? cached-file)
       (begin
         (notify-success)
         (parameterize ([current-eval original-eval])
           (deserialize (call-with-input-file* cached-file fasl->s-exp))))))

(define (cache-compiled! cache-dir path c)
  (define cache-file (cache-dir->file cache-dir))
  (define cache
    (if (file-exists? cache-file)
        (call-with-input-file* cache-file read)
        #hash()))
  (define file-name (sha1 (open-input-bytes (path->bytes path))))
  (define new-cache (hash-set cache (path->string path) file-name))
  (define s (serialize c))
  (make-directory* cache-dir)
  (parameterize ([current-compile original-compile])
    (call-with-output-file*
     #:exists 'truncate
     (build-path cache-dir file-name)
     (lambda (o) (s-exp->fasl s o))))
  (call-with-output-file*
   #:exists 'truncate
   cache-file
   (lambda (o) (writeln new-cache o))))


;; Prime deserialization before any evaluation hooks are set
(require (only-in "main.rkt"
                  [current-namespace prime:current-namespace]
                  [make-empty-core-namespace prime:make-empty-core-namespace]
                  [namespace-syntax-introduce prime:namespace-syntax-introduce]
                  [datum->syntax prime:datum->syntax]
                  [namespace-require prime:namespace-require]
                  [compile prime:compile]
                  [expand prime:expand]))
(define (cache-prime! cache-dir)
  ;; Compile and deserialize enough to cover all serializable struct types
  (void
   (deserialize
    (serialize
     (parameterize ([prime:current-namespace (prime:make-empty-core-namespace)])
       (prime:namespace-require ''#%core)
       (prime:compile
        (prime:expand
         (prime:namespace-syntax-introduce
          (prime:datum->syntax #f '(module m '#%core
                                    (#%provide kar)
                                    (define-values (kar) (quote-syntax car))))))))))))
