#lang racket/base

(provide (struct-out module-use))

(struct module-use (module phase)
        ;; transparent for hashing; note that module path indices will
        ;; be hashed as `equal?`, which makes sense within a module
        #:transparent)
