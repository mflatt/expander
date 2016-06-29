This is a work-in-progress reimplementation of Racket's macro expander.

See "demo.rkt" for examples.

TODO:
 root namespace for reader `dynamc-require`
 module sources and `current-module-path-for-load`
 bug fix for local definition contexts (MB's example)
 load handler's direct loading of submodules
 continuation prompt for module body
 set! on undefined
 enforced namespace constants

----------------------------------------

Running:

 % racket demo.rkt
 or
 % racket bootstrap-demo.rkt

   Runs the examples/tests in "demo.rkt". The tests are not remotely
   complete, but they're a quick and useful sanity check. The
   "demo.rkt" module uses the somewhat internal interface exported by
   `main`, where the expansion, compilation, and evaluation aer less
   overloaded and more controlable.

   Use the "bootstrap-demo.rkt" when running in an older version of
   Racket that is not built with this expander (but that version of
   Racket must be new enough to provide a primitive '#%linket module
   as a bootstrapping hook.)

 % racket run.rkt -c <dir>
 or
 % racket bootstrap-run.rkt -c <dir>

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

   Converts (or checks possibility of converting) a module to a
   stand-alone linklet with no imports, used mainly to extract the
   expander itself. Use `-s` to store source linklets in the cache
   <dir> so that a flattened linklet can be generated; otherwise,
   "run.rkt" will just check that flattening is possible. Use `-o` to
   write the linklet to a file.

----------------------------------------

Roadmap to the implementation:

 syntax/ - syntax-object and binding representation
   syntax.rkt - syntax-object structure
   scope.rkt - scope sets and binding
   binding.rkt - binding representations

 namespace/ - namespaces and module instances

 expand/ - expander loop and core forms

 common/module-path.rkt - [resolved] module path [indexes]

 compile/ - from expanded to S-expression linket

 eval/ - evaluation
   main.rkt - top-level evaluation

 boot/ - internal initialization
   handler.rkt - implements the default module name resolver
   ...-primitive.rkt - export built-in functions as modules

 run/ - helpers to drive the expander

 extract/ - extracts subset of compilation units (by "run.rkt")

 main.rkt - installs eval handler, etc.; entry point for diretcly
            running the expander/compiler/evaluator

 demo.rkt - exercises the expander and compiler (uses "main.rkt")

 run.rkt - starts a Racket replacement (uses "main.rkt")

 bootstrap-run.rkt - like "run.rkt", but for a host Racket that
                     does not include linklet support

 bootstrap-demo.rkt - like "demo.rkt", but for a host Racket that
                      does not include linklet support

Beware that names are routinely shadowed when they are provided by
`racket/base` but replaced by the expander's implementation. For
example, `syntax?` is shadowed, and any part of the expander that
needs `syntax?` must import "syntax/syntax.rkt" or
"syntax/checked-syntax.rkt".

----------------------------------------

Some naming conventions:

 s or stx - a syntax object

 sc - a scope

 scs - a set or list of scopes

 id - an identifier (obviously)

 b - a binding; sometimes spelled out as `binding`

 m - a result of syntax matching

 m - a module

 ns - a namespace

 ctx - an expansion context (including the expand-time environment)

 cctx - a compilation context (including a compile-tme environment)

 insp - an inspector

 mpi - a module path index

 mod-name - a resolved module path, usually; sometimes used for other
  forms of module reference (FIXME)

 <subscript>-<something> - like <something>, but specifically one for
   <subscript>; for example, `m-ns` is a namespace for some module
