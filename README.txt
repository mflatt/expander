This is a work-in-progress reimplementation of Racket's macro expander.

See "demo.rkt" for examples.

TODO:
 namespace-require/copy
 bug fix for local definition contexts (MB's example)

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
   as a bootstrapping hook).

 % racket run.rkt -c <dir>
 or
 % racket bootstrap-run.rkt -c <dir>

   Runs the expander to load itself from source. Expanded and compiled
   modules are stored in <dir>, somewhat like bytecode files.
   Dependency tarcking doesn't take into account the expander itself,
   so throw away <dir> if the expander changes in a way that you want
   reflected in compilation results.

 % racket run.rkt -c <dir> -l <module-path>
 % racket run.rkt -c <dir> -t <file-path-for-module>

   Runs the expander to load the specified module (instead of the
   default module, which is the expander itself).

 % racket run.rkt -c <dir> -f <file-path-for-top-level>

   Loads the given file as a sequence of top-level forms.

 % racket run.rkt -c <dir> -e -l <module-path>

   Expands the given file, instead of compiling and running it.

 % racket run.rkt -c <dir> --linklets -l <module-path>

   Compiles the given file to a set of linklets in S-expression form,
   instead of compiling and running it.

 % racket run.rkt -c <dir> -x

   Checks possibility of converting a module to a stand-alone linklet
   with no imports, used mainly to extract the expander itself.

 % racket bootstrap-run.rkt -c <dir> -o <checkout-dir>/racket

   Compiles the expander to source files in <dir> --- note that
   "bootstrap-run.rkt" must be used to get source compiles --- and
   writes the flattened linklet to "startup.inc" in a Git checkout of
   a linklet-based Racket. Be sure to increment the target Racket
   version if you change the serialization of syntax objects or the
   linklet protocol.

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
   main.rkt - compiler functions called from "eval/main.rkt"

 eval/ - evaluation
   main.rkt - top-level evaluation, with top-level `module` forms as
              an important special case; the `compile-to-linklets`
              function compiles to a set of S-expression linklets
   api.rkt - wrappers that implement `eval`, `compile`, and `expand`
             for `racket/base`

 boot/ - internal initialization
   handler.rkt - implements the default module name resolver, eval
                 handler, and compiler handler
   ...-primitive.rkt - export built-in functions as modules

 run/ - helpers to drive the expander
   linklet.rkt - a bootstrapping implementation of `linklet` by
                 compilation into `lambda` plus primitives

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
