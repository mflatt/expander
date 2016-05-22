#lang racket/base
(require racket/serialize
         racket/file
         file/sha1)

(provide get-cached-compiled
         cache-compiled!)

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
  (and cached-file
       (file-exists? cached-file)
       (begin
         (notify-success)
         (parameterize ([read-accept-compiled #t])
           (call-with-input-file* cached-file read)))))

(define (cache-compiled! cache-dir path c)
  (define cache-file (cache-dir->file cache-dir))
  (define cache
    (if (file-exists? cache-file)
        (call-with-input-file* cache-file read)
        #hash()))
  (define file-name (sha1 (open-input-bytes (path->bytes path))))
  (define new-cache (hash-set cache (path->string path) file-name))
  (make-directory* cache-dir)
  (call-with-output-file*
   #:exists 'truncate
   (build-path cache-dir file-name)
   (lambda (o) (write c o)))
  (call-with-output-file*
   #:exists 'truncate
   cache-file
   (lambda (o) (writeln new-cache o))))
