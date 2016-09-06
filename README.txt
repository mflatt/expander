This simplified variant of the Racket expander demonstrates modules
and submodules.

Relative to the full expander:

 * omits the top level

 * disallows mutual reference across `begin-for-syntax`es

 * disallows shadowing of a module's initial require

 * internal definition contexts and `letrec-syntaxes+values`
   expand to one `letrec-values` form, instead of nested
   `let-values` and `letrec-values` forms

 * no rename transformers, `set!` transformers, syntax taints,
   `local-expand`, `syntax-local-....`, source locations,
   or module path indices

 * non-lazy representations of syntax and module instances

 * omits cross-phase persistent modules

 * fewer representation optimizations (e.g., no bulk
   representation of `require`s)

 * simpler compilation approach (i.e., not linklets) and no
   support for bytecode marshaling

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
