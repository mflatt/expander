This simplified variant of the Racket expander demonstrates the basics
of set-of-scopes expansion.

Relative to the "pico" expander:

 * split into modules and uses more abstraction

 * uses syntax objects for list forms as well as identifiers (and
   uses a custom matcher for syntax internally)

 * normalized `datum->syntax` to accept an existing syntax object that
   supplies the scope set for new syntax

 * identifier macros are allowed (i.e., not constrained to application
   positions)

 * adds `#%app` and expands every application to an `#%app` form

Relative to the "mini" expander:

 * simplified nested syntax: only lists of syntax

 * simpler scope and binding representation

 * omit multiple-identifier binding

 * omits definition contexts

 * omits "rest" arguments in `lambda`

 * adds various Racket forms, such as `case-lambda`

 * omits binding resolution of implicit forms, such as `#%app`

 * no namespace layer (just table of core bindings)

 * no expand-context layer (just an expand-time environment)

----------------------------------------

Roadmap to the main pieces:

 syntax.rkt - syntax-object structure
 
 match.rkt - support library for simple pattern matching

 scope.rkt - scope sets and binding

 binding.rkt - binding representations

 expand[-....].rkt - expander loop and core forms

 compile.rkt - from expanded to raw S-expression

 main.rkt - public interface

 demo.rkt - exercises the expander and compiler
