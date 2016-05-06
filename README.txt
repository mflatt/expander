This is a work-in-progress reimplementation of Racket's macro expander.

See "demo.rkt" for examples.

TODO:
 Racket-style local definition contexts
 shadowing of initial require by other requires
 mutual references in `begin-for-syntax`
 top level
 module path indexes
 module resolver
 cross-phase persistent modules
 lazy module instantiation
 lazy scope propagation
 use-site scope "optimization"
 rename transformers
 source locations
 syntax-local-...
 taints
 lift syntax objects with shifts
 compile to serializable form

----------------------------------------

Roadmap to the main pieces:

 syntax.rkt - syntax-object structure

 scope.rkt - scope sets and binding

 binding.rkt - binding representations

 namespace.rkt - namespaces

 require.rkt - module require and instantiation

 expand[-....].rkt - expander loop and core forms

 expand-{module,require,provide}.rkt - module expander

 compile.rkt - from expanded to raw S-expression

 main.rkt - public interface

 demo.rkt - exercises the expander and compiler
