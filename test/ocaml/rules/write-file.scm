(define proj-root (string-append (getenv "HOME") "/ocaml/ocaml/"))

(define empty
  '((:pkg-path "ocamltest")
    (:rule (rule
            (targets empty.ml)
            (deps (source_tree %{project_root}/runtime/caml))
            (action (write-file %{targets} "(* hack *)"))))))

(define runtime
  '((:pkg-path "runtime")
    (:rule (rule
            (targets runtime.ml)
            (action (write-file %{targets} "let linkme = ()"))))))
