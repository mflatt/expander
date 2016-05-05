This simplified variant of the Racket expander demonstrates the basics
of set-of-scopes expansion in as few lines as reasonable.

Relative to the "micro" expander:

 * strips away some module and abstraction boundaries

----------------------------------------

Roadmap to the main pieces:

 main.rkt - the whole expander and compiler

 match.rkt - support library for simple pattern matching

 demo.rkt - exercises the expander and compiler
