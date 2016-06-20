#lang racket/base
(require "../common/set.rkt"
         "../host/linklet.rkt"
         "../compile/module-use.rkt"
         "../common/module-path.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt")

(provide check-require-access)

(define (check-require-access linklet #:skip-imports skip-num-imports
                              import-module-uses import-module-instances insp
                              extra-inspector
                              extra-inspectorsss)
  (for ([import-syms (in-list (list-tail (linklet-import-variables linklet) skip-num-imports))]
        [mu (in-list import-module-uses)]
        [mi (in-list import-module-instances)]
        [extra-inspectorss (in-list (or extra-inspectorsss
                                        ;; Use `import-module-uses` just to have the right shape
                                        import-module-uses))])
    (define m (module-instance-module mi))
    (unless (module-no-protected? m)
      (define access (or (module-access m) (module-compute-access! m)))
      (for ([import-sym (in-list import-syms)]
            [extra-inspectors (in-list (or (and extra-inspectorsss
                                                extra-inspectorss)
                                           ;; Use `import-syms` just to have the right shape
                                           import-syms))])
        (define a (hash-ref (hash-ref access (module-use-phase mu) #hasheq())
                            import-sym
                            'unexported))
        (when (or (eq? a 'unexported) ; not provided => implicitly protected
                  (eq? a 'protected))
          (define guard-insp (namespace-inspector (module-instance-namespace mi)))
          (unless (or
                   ;; Allowed at declaration time?
                   (inspector-superior? insp guard-insp)
                   ;; Allowed back at compile time?
                   (and extra-inspector (inspector-superior? extra-inspector guard-insp))
                   ;; Allowed by inspectors attached to each referencing syntax object?
                   (and extra-inspectorsss
                        extra-inspectorss
                        (for/and ([extra-insp (in-set extra-inspectors)])
                          (inspector-superior? extra-insp guard-insp))))
            (error 'link
                   (string-append "access disallowed by code inspector to ~a variable\n"
                                  "  variable: ~s\n"
                                  "  from module: ~a")
                   a
                   import-sym
                   (module-path-index-resolve (namespace-mpi (module-instance-namespace mi))))))))))
