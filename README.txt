This simplified variant of the Racket expander demonstrates the basics
of set-of-scopes expansion.

Relative to the "pico" expander:

 * split into modules and uses more abstraction

Relative to the "mini" expander:

 * simpler scope and binding representation

 * omit multiple-identifier binding

 * omits definition contexts

 * omits "rest" arguments in `lambda`

 * adds various Racket forms, such as `case-lambda`

 * omits implicit forms, such as `#%app`

 * no namespace layer (just table of core bindings)

 * no expand-context layer (just an expand-time environment)

----------------------------------------

Roadmap to the main pieces:

 syntax.rkt - syntax-object structure

 scope.rkt - scope sets and binding

 binding.rkt - binding representations

 expand[-....].rkt - expander loop and core forms

 compile.rkt - from expanded to raw S-expression

 main.rkt - public interface

 demo.rkt - exercises the expander and compiler
