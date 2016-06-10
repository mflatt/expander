This is a work-in-progress reimplementation of Racket's macro expander.

See "demo.rkt" for examples.

TODO:
 bug fix for local definition contexts (MB's example)
 load handler's direct loading of submodules
 lazy module instantiation
 preserved syntax properties
 taints
 expander observer
 continuation barrier on macro invocation
 enforced namespace constants

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

 compile[-....].rkt - from expanded to raw S-expression linket

 boot.rkt - implements the default module name resolver

 main.rkt - installs eval handler, compile handler, and resolver

 demo.rkt - exercises the expander and compiler

 run.rkt - starts a Racket replacement

 extract.rkt - extracts subset of compilation units (via "run.rkt")
