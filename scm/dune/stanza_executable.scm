;; https://dune.readthedocs.io/en/stable/dune-files.html#executable

;; (executable
;;  (name <name>)
;;  <optional-fields>)

;; <optional-fields> are:

;; (public_name <public-name>) specifies that the executable should be installed under that name. It is the same as adding the following stanza to your dune file:

;; (install
;;  (section bin)
;;  (files (<name>.exe as <public-name>)))
;; (package <package>) if there is a (public_name ...) field, this specifies the package the executables are part of
;; (libraries <library-dependencies>) specifies the library dependencies. See the section about Library dependencies for more details
;; (link_flags <flags>) specifies additional flags to pass to the linker. This field supports (:include ...) forms
;; (link_deps (<deps-conf list>)) specifies the dependencies used only by the linker, for example when using a version script. See the Dependency specification section for more details.
;; (modules <modules>) specifies which modules in the current directory dune should consider when building this executable. Modules not listed here will be ignored and cannot be used inside the executable described by the current stanza. It is interpreted in the same way as the (modules ...) field of library
;; (root_module <module>) specifies a root_module that collects all dependencies specified in libraries. See the documentation for root_module in the library stanza.
;; (modes (<modes>)) sets the linking modes. The default is (exe). Before 2.0, it used to be (byte exe).
;; (preprocess <preprocess-spec>) is the same as the (preprocess ...) field of library
;; (preprocessor_deps (<deps-conf list>)) is the same as the (preprocessor_deps ...) field of library
;; js_of_ocaml. See the section about js_of_ocaml
;; flags, ocamlc_flags and ocamlopt_flags. See the section about specifying OCaml flags
;; (modules_without_implementation <modules>) is the same as the corresponding field of library
;; (allow_overlapping_dependencies) is the same as the corresponding field of library
;; (optional) is the same as the corresponding field of library
;; (enabled_if <blang expression>) is the same as the corresponding field of library
;; (promote <options>) allows promoting the linked executables to the source tree. The options are the same as for the rule promote mode. Adding (promote (until-clean)) to an executable stanza will cause Dune to copy the .exe files to the source tree and dune clean to delete them
;; (foreign_stubs <foreign-stubs-spec>) specifies foreign source files, e.g. C or C++ stubs, to be linked into the executable. See the section Foreign sources and archives for more details.
;; (foreign_archives <foreign-archives-list>) specifies archives of foreign object files to be linked into the executable. See the section Foreign archives for more details.
;; (forbidden_libraries <libraries>) ensures that the given libraries are not linked in the resulting executable. If they end up being pulled in, either through a direct or transitive dependency, Dune fails with an error message explaining how the library was pulled in. This field is available since the 2.0 version of the dune language.
;; (embed_in_plugin_libraries <library-list>) specifies a list of libraries to link statically when using plugin linking mode. By default, no libraries are linked in. Note that you may need to also use the -linkall flag if some of the libraries listed here are not referenced from any of the plugin modules.

