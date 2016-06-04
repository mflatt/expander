#lang racket/base
(require "binding.rkt")

(provide def-ids-to-binding-syms)
  
(define (def-ids-to-binding-syms ids phase self)
  (for/list ([id (in-list ids)])
    (define b (resolve+shift id phase #:immediate? #t))
    (unless (and (module-binding? b)
                 (eq? self (module-binding-module b))
                 (eqv? phase (module-binding-phase b)))
      (error "bad binding for definition:" id
             self "vs." (and b (module-binding-module b))
             phase))
    (module-binding-sym b)))
