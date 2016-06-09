#lang racket/base
(require "syntax.rkt"
         "scope.rkt"
         "syntax-error.rkt"
         "expand-context.rkt"
         "debug.rkt")

(provide raise-ambigious-error
         syntax-debug-info-string)

(define (raise-ambigious-error id ctx)
  (raise-syntax-error #f
                      "identifier's binding is ambiguous"
                      id #f null
                      (syntax-debug-info-string id ctx)))

;; ----------------------------------------

(define (syntax-debug-info-string s ctx)
  (define info (syntax-debug-info s (expand-context-phase ctx) #t))
  (let loop ([info info] [layer 0])
    (string-append
     "\n  context " (layer->string layer) "...:"
     (describe-context (hash-ref info 'context))
     (apply string-append
            (for/list ([b (hash-ref info 'bindings null)])
              (string-append
               "\n  " (if (hash-ref b 'match? #f) "matching" "other") " binding" (layer->string layer) "...:"
               "\n   " (if (hash-ref b 'local #f)
                           "local"
                           (format "~a" (hash-ref b 'module #f)))
               (describe-context (hash-ref b 'context)))))
     (let ([fallbacks (hash-ref info 'fallbacks null)])
       (apply
        string-append
        (for/fold ([str null]) ([fallback (in-list fallbacks)]
                                [layer (in-naturals layer)])
          (loop fallback layer)))))))

(define (describe-context scopes)
  (define strs
    (let loop ([strs null] [scopes scopes])
      (cond
       [(null? scopes) (reverse strs)]
       [else
        (define str (format " ~a" (car scopes)))
        (if (and (pair? strs)
                 ((+ (string-length str) (string-length (car strs))) . < . 72))
            (loop (cons (string-append (car strs) str)
                        (cdr strs))
                  (cdr scopes))
            (loop (cons str strs)
                  (cdr scopes)))])))
  (cond
   [(null? strs) "\n   [empty]"]
   [else
    (apply string-append (for/list ([str (in-list strs)])
                           (string-append "\n  " str)))]))

(define (layer->string layer)
  (if (zero? layer)
      ""
      (format " at layer ~a" layer)))
