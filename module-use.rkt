#lang racket/base
(provide
 (struct-out module-use))

(struct module-use (module phase)
        #:property prop:equal+hash
        ;; Hash/compare with `eq?` on module part:
        (list (lambda (a b eql?)
                (and (eq? (module-use-module a)
                          (module-use-module b))
                     (eqv? (module-use-phase a)
                           (module-use-phase b))))
              (lambda (a hash-code)
                (and (+ (eq-hash-code (module-use-module a))
                        (eqv-hash-code (module-use-phase a)))))
              (lambda (a hash-code)
                (and (eq-hash-code (module-use-phase a))))))
