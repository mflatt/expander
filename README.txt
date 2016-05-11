This is a work-in-progress reimplementation of Racket's macro expander.

See "demo.rkt" for examples.

TODO:
 extra free-identifier=? equivalences via rename transformers
 Racket-style local definition contexts
 top level
 cross-phase persistent modules
 lazy module instantiation
 lazy scope propagation
 use-site scope "optimization"
 origin tracking
 taints
 expander observer
 lift syntax objects with shifts
 compile to serializable form

----------------------------------------

Roadmap to the main pieces:

 syntax.rkt - syntax-object structure

 scope.rkt - scope sets and binding

 binding.rkt - binding representations

 namespace.rkt - namespaces

 expand[-....].rkt - expander loop and core forms

 expand-{module,require,provide}.rkt - module expander

 require+provide.rkt - require and provide tracking

 module-path.rkt - [resolved] module path [indexes]

 compile.rkt - from expanded to raw S-expression

 main.rkt - public interface to core for demo

 demo.rkt - exercises the expander and compiler

 boot.rkt - starts a Racket replacement
