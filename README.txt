This simplified variant of the Racket expander demonstrates definition
contexts and use-site scopes.

Relative to the "demi" expander:

 * omits modules

 * omits phasing

 * omits general namespaces

Relative to the "micro" expander:

 * more flexible nested syntax: can include (non-list) pairs of syntax

 * more efficient scope and binding representation

 * generalized to multiple-value binding forms

 * adds definition contexts

 * adds various Racket forms, such as `case-lambda`

 * adds implicit forms: `#%app`, `#%top`, and `#%datum`

 * adds stub namespace layer

----------------------------------------

Roadmap to the main pieces:

 syntax.rkt - syntax-object structure

 scope.rkt - scope sets and binding

 binding.rkt - binding representations

 namespace.rkt - ties recursive knot for the expander

 expand[-....].rkt - expander loop and core forms

 compile.rkt - from expanded to raw S-expression

 main.rkt - public interface

 demo.rkt - exercises the expander and compiler
