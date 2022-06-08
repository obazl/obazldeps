(define proj-root (string-append (getenv "HOME") "/ocaml/ocaml/"))

(define toplevel/byte
  '((:pkg-path "toplevel")
    (:stanza
     (copy_files# byte/*.ml))))

(define toplevel/byte2
  '((:pkg-path "toplevel")
    (:stanza
     (copy_files# (files byte/*.ml)))))
