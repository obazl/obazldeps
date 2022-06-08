;; see with-stdout-to.scm

(define ocaml/utils
  '((:pkg-path "utils")
    (:rule (rule
            (targets domainstate.ml)
            (mode    fallback)
            (deps    (:conf ../Makefile.config)
                     (:c domainstate.ml.c)
                     (:tbl ../runtime/caml/domain_state.tbl))
            (action
             (with-stdout-to %{targets}
                             (bash
                              "`grep '^CPP=' %{conf} | cut -d'=' -f2` -I ../runtime/caml %{c} %{tbl}"
                              )))))))

;; see progn.scm
