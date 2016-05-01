#lang racket/base
(require "namespace.rkt"
         "binding.rkt")

(provide (struct-out expand-context)
         current-expand-context)

(struct expand-context (phase      ; current expansion phase
                        namespace  ; namespace for modules and top-levels
                        env        ; environment for local bindings
                        only-immediate? ; #t => stop at core forms
                        add-scope  ; scope to add to every expansion; #f if none
                        ))

(define current-expand-context (make-parameter
                                (expand-context 0
                                                (current-namespace)
                                                empty-env
                                                #f
                                                #f)))
