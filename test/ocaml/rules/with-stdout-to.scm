(define proj-root (string-append (getenv "HOME") "/ocaml/ocaml/"))

(define ocaml/lambda
  '((:pkg-path "lambda")
   (:rule (rule
           (targets runtimedef.ml)
           (mode    fallback)
           (deps    (:fail (file ../runtime/caml/fail.h))
                    (:prim (file ../runtime/primitives)))
           (action  (with-stdout-to %{targets}
                                    (run ./generate_runtimedef.sh %{fail} %{prim})))))))


(define ocaml/parsing
  `((:pkg-path "parsing")
    (:rule (rule
             (targets parser.ml)
             (mode    fallback)
             (deps    (:dep ../boot/menhir/parser.ml))
             (action
              (with-stdout-to %{targets}
                              (bash "cat %{dep} | sed 's/MenhirLib/CamlinternalMenhirLib/g'")))))))

;; (rule
;;  (targets parser.mli)
;;  (mode    fallback)
;;  (deps    (:dep ../boot/menhir/parser.mli))
;;  (action
;;    (with-stdout-to %{targets}
;;      (bash "cat %{dep} | sed 's/MenhirLib/CamlinternalMenhirLib/g'"))))

(define ocaml/runtime
  '((:pkg-path "runtime")
    (:rule (rule
            (targets primitives)
            (mode    fallback)
            (deps
                                        ; matches the line structure of files in gen_primitives.sh
             alloc.c array.c compare.c extern.c floats.c gc_ctrl.c hash.c intern.c
             interp.c ints.c io.c
             lexing.c md5.c meta.c memprof.c obj.c parsing.c signals.c str.c sys.c
             callback.c weak.c
             finalise.c stacks.c dynlink.c backtrace_byt.c backtrace.c
             afl.c
             bigarray.c eventlog.c)
            (action  (with-stdout-to %{targets} (run %{dep:gen_primitives.sh})))))))

(define ocaml/runtime/caml
  '((:pkg-path "runtime/caml")
    (:rule (rule
            (targets version.h)
            (mode    fallback)
            (action
             (with-stdout-to %{targets}
                             (run %{dep:../../tools/make-version-header.sh} %{dep:../../VERSION})))))))

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

;; (rule
;;  (targets domainstate.mli)
;;  (mode    fallback)
;;  (deps    (:conf ../Makefile.config)
;;           (:c domainstate.mli.c)
;;           (:tbl ../runtime/caml/domain_state.tbl))
;;  (action
;;    (with-stdout-to %{targets}
;;      (bash
;;        "`grep '^CPP=' %{conf} | cut -d'=' -f2` -I ../runtime/caml %{c} %{tbl}"
;;        ))))
