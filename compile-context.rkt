#lang racket/base
(require "namespace.rkt")

(provide (struct-out compile-context)
         make-compile-context)

(struct compile-context (namespace   ; compile-time namespace
                         phase       ; phase (top level) or phase level (within a module)
                         self        ; if non-#f module path index, compiling the body of a module
                         compile-time-for-self ; for allowing forward references across `begin-for-syntax`
                         root-module-name ; set to a symbol if `self` is non-#f
                         header))    ; accumulates initialization and other parts shared among expressions

(define (make-compile-context #:namespace [namespace (current-namespace)]
                              #:phase [phase (namespace-phase namespace)]
                              #:self [self #f]
                              #:compile-time-for-self [compile-time-for-self #f]
                              #:root-module-name [root-module-name #f])
  (when (and self (not root-module-name))
    (error "internal error: self provided without root"))
  (compile-context namespace 
                   phase
                   self
                   compile-time-for-self
                   root-module-name
                   #f))
