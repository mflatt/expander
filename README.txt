This is a work-in-progress reimplementation of Racket's macro expander.

See "demo.rkt" for examples.

TODO:
 bug fix for local definition contexts (MB's example)
 load handler's direct loading of submodules
 preserved syntax properties
 expander observer
 continuation barrier on macro invocation
 continuation prompt for module body
 enforced namespace constants
 lock on namespace & registry

----------------------------------------

Roadmap to a few key pieces:

 syntax/ - syntax-object and binding representation
   syntax.rkt - syntax-object structure
   scope.rkt - scope sets and binding
   binding.rkt - binding representations

 namespace/ - namespaces and module instances

 expand/ - expander loop and core forms

 common/module-path.rkt - [resolved] module path [indexes]

 compile/ - from expanded to S-expression linket

 eval/ - evaluation
   main.rkt - installs eval handler, compile handler, and resolver
   eval.rkt - top-level evaluation

 boot/ - internal initialization
   handler.rkt - implements the default module name resolver
   ...-primitive.rkt - export built-in functions as modules

 run/ - helpers to drive the expander
   extract.rkt - extracts subset of compilation units (via "run.rkt")

 main.rkt - expander/compiler/evaluator entry point

 demo.rkt - exercises the expander and compiler (uses "main.rkt")

 run.rkt - starts a Racket replacement (uses "main.rkt")

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
