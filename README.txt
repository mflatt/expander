This simplified variant of the Racket expander demonstrates the basics
of set-of-scopes expansion in as few lines as reasonable.

Relative to the "micro" expander:

 * strips away some module and abstraction boundaries

 * use syntax objects only for identifiers

 * simplifies `datum->syntax` to always create syntax with an empty
   scope set

Relative to the "pico" expander:

 * multi-argument functions and multi-binding `let-syntax`

 * add `bound-identifier=?` and `free-identifier=?`

 * add implicit quoting

 * accomodate non-function compile-time bindings

 * rename `introduce` to `namespace-syntax-introduce

----------------------------------------

Roadmap:

 main.rkt - the whole expander and compiler

 demo.rkt - exercises the expander and compiler
