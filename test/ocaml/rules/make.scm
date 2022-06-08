;; ocaml/ocamltest/dune

(rule
 (targets ocamltest_config.ml)
 (deps
   ../Makefile.config
   ../Makefile.build_config
   ../Makefile.config_if_required
   ../Makefile.common
   ../Makefile.best_binaries
   Makefile
   ./ocamltest_config.ml.in)
 (action (run make %{targets} COMPUTE_DEPS=false)))

;; ocaml/utils/dune

(rule
 (targets config.ml)
 (mode    fallback)
 (deps    (:mk Makefile)
          ../Makefile.config
          ; for now the utils Makefile does not use build_config
          config.mlp)
 (action  (system "make -f %{mk} %{targets}")))
