# dune notes

## dune files

Projects may put dune code in files other than `dune`, e.g. for inclusion in the main `dune` file. Example:

`src/lib_protocol_environment/structs/dune` contains several `include`
stanzas of the form `(include v0.dune.inc)`.

Since such included files may contain `library` etc. stanzas we need
to process them with codept.

## empty `(module)`

`tezos/src/lib_protocol_environment/s_packer/dune`

```
(library
 (name tezos_protocol_environment_packer)
 (public_name tezos-protocol-environment-packer)
 (modules))

(executable
 (name s_packer))

(install
  (section libexec)
  (package tezos-protocol-environment-packer)
  (files (s_packer.exe as s_packer)))
```

Apparently the `install` stanza depends on the package, just so it can install the executable at `<prefix>/tezos-protocol-environment-packer/s_packer`

Since Bazel does not install things, we can ignore this and just
depend on target `s_packer` where needed.

However, the package is used be e.g. `tezos/src/lib_protocol_environment/structs/v0.dune.inc`:

`(run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))`

Note that `libexec` ties these two stanzas together

For Bazel we need to convert this to a genrule with tool `//src/lib_protocol_environment:s_packer`

## dune src code:

`src/dune_rules/dune_file.ml`:

```
module Buildable = struct
  type t =
    { loc : Loc.t
    ; modules : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; foreign_archives : (Loc.t * Foreign.Archive.t) list
    ; foreign_stubs : Foreign.Stubs.t list
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
    ; flags : Ocaml_flags.Spec.t
    ; js_of_ocaml : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    ; root_module : (Loc.t * Module_name.t) option
    }
```

`editor-integration/emacs/dune.el`:

```
(defconst dune-fields-regex
  (eval-when-compile
    (regexp-opt
     '("name" "public_name" "synopsis" "modules" "libraries" "wrapped"
       "preprocess" "preprocessor_deps" "optional" "c_names" "cxx_names"
       "foreign_stubs" "foreign_archives" "install_c_headers" "modes"
       "no_dynlink" "kind" "ppx_runtime_libraries" "virtual_deps" "js_of_ocaml"
       "flags" "ocamlc_flags" "ocamlopt_flags" "library_flags" "c_flags"
       "cxx_flags" "c_library_flags" "self_build_stubs_archive" "inline_tests"
       "modules_without_implementation" "private_modules"
       ;; + special_builtin_support
       "special_builtin_support" "build_info" "data_module" "api_version"
       ;; +stdlib
       "stdlib" "modules_before_stdlib" "exit_module" "internal_modules"
       ;; + virtual libraries
       "virtual_modules" "implements" "variant" "default_implementation"
       "allow_overlapping_dependencies"
       ;; + for "executable" and "executables":
       "package" "link_flags" "link_deps" "names" "public_names" "variants"
       "forbidden_libraries"
       ;; + for "foreign_library" and "foreign_stubs":
       "archive_name" "language" "names" "flags" "include_dirs" "extra_deps"
       ;; + for "rule":
       "targets" "action" "deps" "mode" "fallback" "locks"
       ;; + for "menhir":
       "merge_into"
       ;; + for "cinaps":
       "files"
       ;; + for "alias"
       "enabled_if"
       ;; + for env
       "binaries"
       ;; + for "install"
       "section" "files")
     'symbols))
  "Field names allowed in dune files.")
```
