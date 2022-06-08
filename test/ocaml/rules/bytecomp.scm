(rule
 (targets opcodes.ml)
 (mode    fallback)
 (deps    (:instr (file ../runtime/caml/instruct.h)))
 (action
  (bash "%{dep:../tools/make_opcodes.exe} -opcodes < %{instr} > %{targets}")))
