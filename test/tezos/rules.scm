;; lib_client_base:

(define bip39
  `(rule
    (targets bip39_english.ml)
    (deps    (:exe gen/bip39_generator.exe)
             gen/bip39_english.txt)
    (action  (run %{exe} %{targets}))))

;; lib_protocol_compiler
(define embedded_cmis
  '(rule
    (targets embedded_cmis.ml)
    (action
     (run %{bin:ocp-ocamlres} -format ocaml -o %{targets}
          %{lib:stdlib:camlinternalFormatBasics.cmi}
          %{dep:.tezos_protocol_registerer.objs/byte/tezos_protocol_registerer__Registerer.cmi}
          %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs.cmi}
          %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V0.cmi}
          %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V1.cmi}
          %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V2.cmi}))))

;; lib_protocol_compiler/test
(rule
  (alias runtest_rejections)
  (deps (package tezos-protocol-demo-noops))
  (action
    (run
       bash
         %{dep:rejections.sh}
         %{bin:tezos-protocol-compiler}
         %{lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL}
       )))

(rule
 (alias runtest_out_of_opam)
 (deps
   (alias runtest_rejections))
 (action (progn)))

;; lib_sapling/bindings
(rule
 (targets rustzcash_ctypes_stubs.ml rustzcash_ctypes_c_stubs.c)
 (deps    (:gen ./rustzcash_ctypes_gen.exe))
 (action  (run %{gen} %{targets})))

;; lib_store/legacy_store/test
(rule
 (alias runtest)
 (package tezos-legacy-store)
 (action (chdir %{workspace_root} (run %{exe:test.exe}))))


;; bin_client
(rule
 (action
  (progn
   (write-file void_for_linking-genesis.empty "")
   (write-file void_for_linking-genesis.empty "")
   (write-file void_for_linking-genesis-carthagenet.empty "")
   (write-file void_for_linking-000-Ps9mPmXa.empty "")
   (write-file void_for_linking-001-PtCJ7pwo.empty "")
   (write-file void_for_linking-002-PsYLVpVv.empty "")
   (write-file void_for_linking-003-PsddFKi3.empty "")
   (write-file void_for_linking-004-Pt24m4xi.empty "")
   (write-file void_for_linking-005-PsBabyM1.empty "")
   (write-file void_for_linking-006-PsCARTHA.empty "")
   (write-file void_for_linking-007-PsDELPH1.empty "")
   (write-file void_for_linking-008-PtEdo2Zk.empty "")
   (write-file void_for_linking-009-PsFLoren.empty "")
   (write-file void_for_linking-010-PtGRANAD.empty "")
   (write-file void_for_linking-alpha.empty "")
   (write-file void_for_linking-demo-counter.empty "")
   (write-file void_for_linking-baking-alpha.empty "")
   (write-file void_for_linking-baking-008-PtEdo2Zk.empty "")
   (write-file void_for_linking-baking-009-PsFLoren.empty "")
   (write-file void_for_linking-baking-010-PtGRANAD.empty "")
   (write-file void_for_linking-007-PsDELPH1-protocol-plugin.empty "")
   (write-file void_for_linking-008-PtEdo2Zk-protocol-plugin.empty "")
   (write-file void_for_linking-009-PsFLoren-protocol-plugin.empty "")
   (write-file void_for_linking-010-PtGRANAD-protocol-plugin.empty "")
   (write-file void_for_linking-alpha-protocol-plugin.empty "")
)))

