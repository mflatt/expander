#lang racket/base
(require "compile-context.rkt"
         "compile-top.rkt"
         "compile-module.rkt")

;; Compilation uses one of two protocols, which differ in the shapes
;; of linklets that they generate:
;;
;; * Top-level forms or stand-alone expressions (such as the
;;   right-hand side of a `define-syntaxes` form within a module,
;;   which must be compiled to continue expanding the module) are
;;   compiled using one protocol.
;;
;;   In the case of top-level forms, a sequence of forms that affect
;;   binding or transformers must be compiled separately --- normally
;;   via `per-top-level` in "eval.rkt". The separarately compiled
;;   forms can them be combined into a single compilation record.
;;
;;   The generated linklets for a single form include one linklet for
;;   potentially marshaled module path indices and syntax objects,
;;   plus one linklet per relevant phase. Multi-form combinations
;;   group the linklet sets for individual compilations in nested
;;   linklet directories.
;;
;; * Modules are compiled to a slightly different protocol. Like the
;;   top-level protocol, the resulting set of linklets includes on
;;   linklet per phase plus one linklet for housing potentially
;;   marshaled data. An additional linklet reports metadata about the
;;   modules, such as its requires, provides, and submodule names.
;;   Submodules are represented by nested linklet directories.
;;
;;   Besides the extra metadata module, the handling of syntax-object
;;   unmarshaling is a little different for modules than top-level
;;   forms. Each phase has its own unmarshaling code, and the shared
;;   linklet to house the data is filled on demand with syntax objects
;;   by the per-phase linklets.
;;
;; Whichever protocol is used, the result is wrapped in a
;; `compiled-in-memory` structure, which retains original module path
;; indices and syntax objects. If the compiled code is evaluated
;; directory, then the retained values are used instead of running
;; unmarshaling code in generated linklets. That's both faster an
;; preserves some expected sharing. When a `compile-in-memory`
;; structure is written, it writes the same as a linklet directory.

(provide make-compile-context

         compile-single
         compile-top
         compiled-tops->compiled-top

         compile-module)
