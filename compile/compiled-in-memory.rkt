#lang racket/base

;; A `compiled-in-memory` structure holds the result of compilation.
;; It's produced by `compile-top` or `compile-module` and consumed by
;; `eval-compiled-in-memory`. The marshaled form is just the linklet
;; directory, which has all the same information, but loses sharing
;; with anything else currently in memory.
(provide (struct-out compiled-in-memory))

(struct compiled-in-memory (linklet-directory ;; includes content of `{pre,post}-compiled-tops`
                            ;; Shortcuts, instead of using the metadata linklet:
                            phase
                            max-phase
                            phase-to-link-module-uses
                            ;; Maybe provide more capability than the module's declaration inspector:
                            compile-time-inspector
                            phase-to-link-extra-inspectorsss
                            ;; For using existing values directly, instead of unmarshaling:
                            mpis
                            syntax-literalss
                            ;; Shortcuts for associated code (submodules or sequence of top levels)
                            pre-compiled-in-memorys
                            post-compiled-in-memorys)
        #:property prop:custom-write
        (lambda (cim port mode)
          (write (compiled-in-memory-linklet-directory cim) port)))
