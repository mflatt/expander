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

Roadmap to a few key pieces:

 syntax.rkt - syntax-object structure

 scope.rkt - scope sets and binding

 binding.rkt - binding representations

 namespace.rkt - namespaces

 expand[-....].rkt - expander loop and core forms

 module-path.rkt - [resolved] module path [indexes]

 compile[-....].rkt - from expanded to S-expression linket

 eval[-...].rkt - top-level and module evaluation

 boot.rkt - implements the default module name resolver

 main.rkt - installs eval handler, compile handler, and resolver

 demo.rkt - exercises the expander and compiler

 run.rkt - starts a Racket replacement

 extract.rkt - extracts subset of compilation units (via "run.rkt")

----------------------------------------

Running:

 % racket demo.rkt

   Runs the examples/tests in "demo.rkt". The tests are not remotely
   complete, but they're a quick and useful sanity check. The
   "demo.rkt" module uses the somewhat internal interface exported by
   `main`, where the expansion, compilation, and evaluation aer less
   overloaded and more controlable.

 % racket run.rkt -c <dir>

   Runs the expander to load itself from source. Expanded and compiled
   modules are stored in <dir>, somewhat like bytecode files, but
   there's not automatically dependency tracking; throw away <dir> if
   the expander or any source changes.

 % racket run.rkt -c <dir> -l <module-path>
 % racket run.rkt -c <dir> -t <file-path-for-module>

   Runs the expander to load the specified module (instead of the
   default module, which is the expander itself).

 % racket run.rkt -c <dir> -f <file-path-for-top-level>

   Loads the given file as a sequence of top-level forms.

 % racket run.rkt -c <dir> -e -l <module-path>

   Expands the given file, instead of compiling and running it.

 % racket run.rkt -c <dir> -x

   Reports extraction for bootstrap. Currently, there will be
   failures, because the runtime system doesn't yet implement linklets
   and interfaces, and because the runtime system's `read-syntax` and
   simple syntax-manipulation functions are used.
