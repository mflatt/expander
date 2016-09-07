This simplified variant of the Racket expander demonstrates the basics
of set-of-scopes expansion in as few lines as reasonable.

Relative to the "nano" expander:

 * single-argument functions and single-binding `let-syntax`

 * omits `bound-identifier=?` and `free-identifier=?`

 * no implicit quoting

 * assume compile-time binding to a macro function

 * `namespace-syntax-introduce` is called just `introduce`

----------------------------------------

Roadmap:

 main.rkt - the whole expander and compiler

 demo.rkt - exercises the expander and compiler
