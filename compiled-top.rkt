#lang racket/base

;; A `compiled-top` structure holds the result of compilation for a
;; stand-alone expression or top-level form. It't produced by
;; `compile-top` in "compile.rkt" and consumed by `eval-compiled-top`
;; in "eval-top.rkt".
(provide (struct-out compiled-top))

(struct compiled-top (linklet-directory
                      phase
                      max-phase
                      phase-to-link-module-uses
                      mpis
                      syntax-literalss))
