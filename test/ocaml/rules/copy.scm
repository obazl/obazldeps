(define proj-root (string-append (getenv "HOME") "/ocaml/ocaml/"))

;; ocaml/dune
(define ocamlc.byte
  '((:pkg-path ".")
    (:rule
     (rule
      (copy main.exe ocamlc.byte)))))

(define optmain.exe
  '((:pkg-path ".")
    (:rule
     (rule
      (copy optmain.exe ocamlopt.byte)))))

;; ocaml/lambda/dune

(define lambda/main.exe
  '((:pkg-path "lambda")
    (:rule
     (rule (copy main.exe ocamltest.byte)))))

;; ocaml/parsing/dune:
(define lambda/camlinternalMenhirLib.ml
  '((:pkg-path "lambda")
    (:rule
     (rule
      (targets camlinternalMenhirLib.ml)
      (mode    fallback)
      (action  (copy# ../boot/menhir/menhirLib.ml %{targets}))))
    (:normalized
     '(:run-cmd (:out camlinternalMenhirLib.ml)
               (:cmd ((:tool copy#)
                      (:deps ())
                      (:args ((:_srcfile "./boot/menhir" "menhirLib.ml")
                              (:targets camlinternalMenhirLib.ml)))
                      (:raw ((copy# ../boot/menhir/menhirLib.ml %{targets})))))
               (:vars #f)
               (:raw (rule
                      (targets camlinternalMenhirLib.ml)
                      (mode fallback)
                      (action (copy# ../boot/menhir/menhirLib.ml %{targets})))))))

(define lambda/camlinternalMenhirLib.mli
  '((:pkg-path "lambda")
    (:rule
     (rule
      (targets camlinternalMenhirLib.mli)
      (mode    fallback)
      (action  (copy# ../boot/menhir/menhirLib.mli %{targets}))))))

;; ocaml/runtime/dune
(define lambda/libruntime_stubs.a
  '((:pkg-path "runtime")
    (:rule
     (rule
      (targets libruntime_stubs.a)
      (action (copy libcamlrun.a %{targets}))))
    (:normalized
     (:run-cmd (:out libruntime_stubs.a)
               (:cmd ((:tool copy)
                      (:deps ())
                      (:args ((:_genfile "runtime" "libcamlrun.a") (:targets libruntime_stubs.a)))
                      (:raw ((copy libcamlrun.a %{targets})))))
               (:vars #f)
               (:raw (rule
                      (targets libruntime_stubs.a)
                      (action (copy libcamlrun.a %{targets}))))))))

;; ocaml/toplevel/dune
;;  not a rule stanza:
;; (define toplevel/byte
;;   '((:pkg-path "lambda")
;;     (:rule
;;      (copy_files# byte/*.ml))))
