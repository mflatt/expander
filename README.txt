This is a work-in-progress reimplementation of Racket's macro expander.

See "demo.rkt" for examples.

TODO:
 Racket-style local definition contexts
 top level
 lazy module instantiation
 origin tracking
 taints
 expander observer
 expand local references to binding
 prune unreachable scopes on serialize

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
