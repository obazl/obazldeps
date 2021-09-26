(display "loading ocamlark_test.scm") (newline)

(define (hash-table-keys ht)
  (map car ht))

(define (analyze-file filename)

  ;; run codept to create .obazl.d/codept.deps

  ;; test:
  ;; always already loaded: (load "ocamlark.scm")
  (let ((depsfile ".obazl.d/codept.deps"))
    (ocamlark-handler depsfile)
  ))
