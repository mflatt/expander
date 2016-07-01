#lang racket/base
(require "../eval/dynamic-require.rkt"
         "host-syntax-to-syntax.rkt")

(provide dynamic-require-reader)

(define (dynamic-require-reader mod-path sym)
  (define proc (dynamic-require mod-path sym))
  (cond
   [(and (eq? sym 'read-syntax)
         (procedure-arity-includes? proc 6))
    ;; Need to convert syntax object for module name
    (lambda (name input mod-s line column position)
      ;; We don't try to convert the result; if it's syntax, it will get
      ;; wrapped again as a host syntax object by the reader, but that
      ;; extra wrapper is harmlessly removed by our `read-syntax`; we
      ;; rely on that same coincidence for readtables
      (proc name input (host-syntax->syntax mod-s) line column position))]
   [else proc]))
