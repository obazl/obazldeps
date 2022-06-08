(define proj-root (string-append (getenv "HOME") "/tweag/tezos"))

(define alias/runtest_rejections
  '((:pkg-path "src/lib_protocol_compiler/test")
    (:rule (rule
            (alias runtest_rejections)
            (deps (package tezos-protocol-demo-noops))
            (action
             (run
              bash
              %{dep:rejections.sh}
              %{bin:tezos-protocol-compiler}
              %{lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL}
              ))))))

