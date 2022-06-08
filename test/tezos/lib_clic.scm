(define proj-root (string-append (getenv "HOME") "/tweag/tezos"))

;; starlark:  ocaml_test
(define lib_clic/executables
  '((:pkg-path "src/lib_clic/test")
    (:stanza
     (executables
      (names test_clic)
      (libraries tezos-clic
                 alcotest-lwt)
      (flags (:standard -open Tezos_stdlib
                        -open Tezos_clic))))))

;; starlark: not needed
(define lib_clic/buildtest
  '((:pkg-path "src/lib_clic/test")
    (:rule (rule
            (alias buildtest)
            (deps test_clic.exe)
            (action (progn))))
    (:normalized
     (:run-cmd ((:alias runtest_clic)
                (:cmd ((:tool ((:_exe "." "test_clic.exe")))
                       (:raw ((run %{exe:test_clic.exe})))))
                (:pkg "src/lib_clic/test")
                (:raw (rule (alias runtest_clic)
                            (action (run %{exe:test_clic.exe})))))))))

;; starlark: not needed
(define lib_clic/runtest_clic
  '((:pkg-path "src/lib_clic/test")
    (:rule (rule
            (alias runtest_clic)
            (action (run %{exe:test_clic.exe}))))
    (:normalized
     (:run-cmd ((:alias runtest_clic)
                (:cmd ((:tool ((:_exe "." "test_clic.exe")))
                       (:raw ((run %{exe:test_clic.exe})))))
                (:pkg "src/lib_clic/test")
                (:raw (rule (alias runtest_clic)
                            (action (run %{exe:test_clic.exe})))))))))

;; starlark: not needed
(define lib_clic/runtest
  '((:pkg-path "src/lib_clic/test")
    (:rule (rule
            (alias runtest)
            (package tezos-clic)
            (deps (alias runtest_clic))
            (action (progn))))
    (:normalized
     (:null-cmd ((:alias runtest)
                 (:deps (:_alias runtest_clic))
                 (:pkg "src/lib_clic/test")
                 (:raw (rule (alias runtest)
                             (package tezos-clic)
                             (deps (alias runtest_clic))
                             (action (progn)))))))))
