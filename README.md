# ocamldeps
OCaml project dependency analysis. C11 wrapper on [codept](https://github.com/Octachron/codept).

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

