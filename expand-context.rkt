#lang racket/base
(require "namespace.rkt"
         "binding.rkt")

(provide (struct-out expand-context)
         make-expand-context)

(struct expand-context (use-site-scopes ; #f or boxed list: scopes that should be pruned from binders
                        namespace  ; namespace for top-levels
                        env        ; environment for local bindings
                        only-immediate? ; #t => stop at core forms
                        post-expansion-scope ; scope to add to every expansion; #f if none
                        ))


(define (make-expand-context ns)
  (expand-context #f ; use-site scopes
                  ns
                  empty-env
                  #f
                  #f))
