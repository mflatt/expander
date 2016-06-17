#lang racket/base
(require "syntax.rkt"
         "scope.rkt")

(provide syntax-track-origin)

(define missing (gensym))

(define (syntax-track-origin new-stx old-stx [id (if (identifier? old-stx)
                                                     old-stx
                                                     (car (syntax-e/no-taint old-stx)))])
  (define old-props (syntax-props old-stx))
  (cond
   [(zero? (hash-count old-props))
    (syntax-property new-stx 'origin (list id))]
   [else
    (define new-props (syntax-props new-stx))
    (cond
     [(zero? (hash-count new-props))
      (define old-origin (hash-ref old-props 'origin missing))
      (define origin (if (eq? old-origin missing)
                         (list id)
                         (cons id old-origin)))
      (struct-copy syntax new-stx
                   [props (hash-set old-props 'origin origin)])]
     [else
      ;; Merge properties
      (define old-props-with-origin
        (hash-set old-props 'origin (cons id (hash-ref old-props 'origin null))))
      (define updated-props
        (cond
         [((hash-count old-props-with-origin) . < . (hash-count new-props))
          (for/fold ([new-props new-props]) ([(k v) (in-hash old-props-with-origin)])
            (define new-v (hash-ref new-props k missing))
            (hash-set new-props k (if (eq? new-v missing)
                                      v
                                      (cons new-v v))))]
         [else
          (for/fold ([old-props old-props-with-origin]) ([(k v) (in-hash new-props)])
            (define old-v (hash-ref old-props k missing))
            (hash-set old-props k (if (eq? old-v missing)
                                      v
                                      (cons v old-v))))]))
      (struct-copy syntax new-stx
                   [props updated-props])])]))

(module+ test
  (define (check-track new-props old-props expected-props-except-origin)
    (define old-id (datum->syntax #f 'old))
    (define result-props (syntax-props
                          (syntax-track-origin
                           (struct-copy syntax (datum->syntax #f 'new)
                                        [props new-props])
                           (struct-copy syntax (datum->syntax #f (list old-id))
                                        [props old-props]))))
    (unless (equal? result-props
                    (hash-update expected-props-except-origin 'origin
                                 (lambda (v)
                                   (if v
                                       (cons (list old-id) v)
                                       (list old-id)))
                                 #f))
      (error "failed" new-props old-props result-props)))
  
  (check-track (hasheq 'a 1 'b 2)
               (hasheq)
               (hasheq 'a 1 'b 2))
  
  (check-track (hasheq)
               (hasheq 'a 3)
               (hasheq 'a 3))
  
  (check-track (hasheq 'a 1 'b 2)
               (hasheq 'a 3)
               (hasheq 'a (cons 1 3) 'b 2))
  
  (check-track (hasheq 'a 3)
               (hasheq 'a 1 'b 2)
               (hasheq 'a (cons 3 1) 'b 2))
  
  (check-track (hasheq 'a 3)
               (hasheq 'a 1 'b 2)
               (hasheq 'a (cons 3 1) 'b 2)))
