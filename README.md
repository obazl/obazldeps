# ocamlark
ocamlark turns OCaml source files into Starlark build files. Uses [codept](https://github.com/Octachron/codept) to analyze dependencies.

## tools

* `//repl`
* `//runner`

## data structures and names

* files are represented as alists, (:key pkgpath basename)

** :key is `:_srcfile` or `:_genfile`, the former for files that
already exist. If we have a file name (e.g. "test_clic.exe") we check
to see if there is a file by that name; if not, it must be generated,
so `:_genfile`.

## repl workflow

1. Build the repl: `$ bazel build repl`
2. symlink (or copy) the repl to a dir on the path:
   $ ln -fs `realpath bazel-bin/repl/repl` ~/bin/camlark
3. execute the repl; pass -v for verbose

The load path is configured to contain:

1. `$(PWD)/.obazl.d/scm`
2. `$(HOME)/.obazl.d/scm`
3. `$(HOME)/.local/share/obazl/scm`
4. `$(HOME)/.local/share/s7`
5. `.`

Symlink //scm/s7 to `$(HOME)/.local/share/s7`, which is on the load
path. Then any changes in the camlark repo will be live.

We do *not* put `@camlark//scm/s7` or anything else in the camlark
repo on the load path, since that would not work if the user executes
the repl directly instead of running `bazel run repl`.

[TODO: do put them in runfiles if repl invoke by bazel run?]

[TODO: deployment logic to install stuff into XDG ~/.local/share]

* run the repl (copied to ~/bin/camlark)
* `*load-path*` prints the directories s7 searches when loading a file
* `(load "fname.scm")` - load filenames, not 'modules'

In emacs `M-x run-scheme`



## batch workflow

Using `//runner` to batch execute a script.

## whole-project analysis

0. get proj root

1. list OPAM directories

2. list all project source files for dep analysis

3. other local resources?

4. emit_codept_args to args file

    a. emit_codept_src_files
    b. emit_codept_opam_dirs

args file:

-sexp
... list of src files ...
-L
/Users/gar/.opam/4.09.0/lib
...

5. run codept with args file

6. parse output of codept

