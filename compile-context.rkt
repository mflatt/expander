#lang racket/base
(require "namespace.rkt")

(provide (struct-out compile-context)
         make-compile-context)

(struct compile-context (namespace   ; compile-time namespace
                         phase       ; phase (top level) or phase level (within a module)
                         self        ; if non-#f module path index, compiling the body of a module
                         module-self ; if non-#f, same as `self` and compiling the body of a module
                         root-module-name ; set to a symbol if `self` is non-#f
                         header))    ; accumulates initialization and other parts shared among expressions

(define (make-compile-context #:namespace [namespace (current-namespace)]
                              #:phase [phase (namespace-phase namespace)]
                              #:self [self (namespace-mpi namespace)]
                              #:module-self [module-self #f]
                              #:compile-time-for-self [compile-time-for-self #f]
                              #:root-module-name [root-module-name #f])
  (when (and module-self (not root-module-name))
    (error "internal error: module-self provided without root"))
  (compile-context namespace 
                   phase
                   self
                   module-self
                   root-module-name
                   #f))
