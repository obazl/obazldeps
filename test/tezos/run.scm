(define proj-root (string-append (getenv "HOME") "/tweag/tezos"))

(define embedded_cmis
  '((:pkg-path "src/lib_protocol_compiler")
    (:rule (rule
            (targets embedded_cmis.ml)
            (action
             (run %{bin:ocp-ocamlres} -format ocaml -o %{targets}
                  %{lib:stdlib:camlinternalFormatBasics.cmi}
                  %{dep:.tezos_protocol_registerer.objs/byte/tezos_protocol_registerer__Registerer.cmi}
                  %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs.cmi}
                  %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V0.cmi}
                  %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V1.cmi}
                  %{lib:tezos-protocol-environment-sigs:tezos_protocol_environment_sigs__V2.cmi}))))))
