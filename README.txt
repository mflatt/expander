This simplified variant of the Racket expander demonstrates modules
and submodules.

Relative to the full expander:

 * omits the top level

 * disallows mutual reference across `begin-for-syntax`es

 * disallows shadoing of a module's initial require

 * omit cross-phase persistent modules

 * no rename transformers, syntax taints, `local-expand`,
   `syntax-local-....`, source locations, module path indices

 * non-lazy representations of syntax and module instances

Relative to the "mini" expander:

 * adds modules

 * adds phasing

 * adds general namespaces

----------------------------------------

Roadmap to the main pieces:

 syntax.rkt - syntax-object structure

 scope.rkt - scope sets and binding

 binding.rkt - binding representations

 namespace.rkt - namespaces and modules

 expand[-....].rkt - expander loop and core forms

 expand-{module,require,provide}.rkt - module expander

 require+provide.rkt - require and provide tracking

 compile.rkt - from expanded to raw S-expression

 main.rkt - public interface

 demo.rkt - exercises the expander and compiler
