(define proj-root (string-append (getenv "HOME") "/tweag/tezos"))

(define alias/runtest_out_of_opam
  '((:pkg-path "src/lib_protocol_compiler/test")
    (:rule (rule
            (alias runtest_out_of_opam)
            (deps
             (alias runtest_rejections))
            (action (progn))))))

