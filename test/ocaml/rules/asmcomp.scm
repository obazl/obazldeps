(begin
  (load "alist.scm")
  (load "dune.scm")
  (load "dune_stanzas.scm")
  (load "dune_stanza_rule.scm")
  (load "utils.scm"))

(define rule1
  '(rule
    (targets arch.ml CSE.ml proc.ml reload.ml scheduling.ml selection.ml)
    (mode    fallback)
    (deps    (:conf ../Makefile.config)
             (glob_files amd64/*.ml)
             (glob_files arm/*.ml)
             (glob_files arm64/*.ml)
             (glob_files i386/*.ml)
             (glob_files power/*.ml)
             (glob_files riscv/*.ml)
             (glob_files s390x/*.ml))
    (action  (bash "cp `grep '^ARCH=' %{conf} | cut -d'=' -f2`/*.ml ."))))

(define rule2
  '(rule
    (targets emit.ml)
    (mode    fallback)
    (deps    (:conf ../Makefile.config)
             amd64/emit.mlp
             arm/emit.mlp
             arm64/emit.mlp
             i386/emit.mlp
             power/emit.mlp
             riscv/emit.mlp
             s390x/emit.mlp)
    (action
     (progn
      (with-stdout-to contains-input-name
                      (bash "echo `grep '^ARCH=' %{conf} | cut -d'=' -f2`/emit.mlp"))
      (with-stdout-to %{targets}
                      (progn
                       (bash "echo \\# 1 \\\"`cat contains-input-name`\\\"")
                       (bash "%{dep:../tools/cvt_emit.exe} < `cat contains-input-name`")))))))

(define pkg-path "asmcomp")
(define srcfiles (directory->list pkg-path))

rule1
(load "dune_stanza_rule.scm")
(load "dune_action
(define ns (normalize-stanza-rule pkg-path
                                  srcfiles
                                  rule1))

ns

;; NORMALIZE-STANZA-RULE: ./asmcomp

;; stanza: (rule (targets emit.ml) (mode fallback) (deps (:conf ../Makefile.config) amd64/emit.mlp arm/emit.mlp arm64/emit.mlp i386/emit.mlp power/emit.mlp riscv/emit.mlp s390x/emit.mlp) (action (progn (with-stdout-to contains-input-name (bash "echo `grep '^ARCH=' %{conf} | cut -d'=' -f2`/emit.mlp")) (with-stdout-to %{targets} (progn (bash "echo \\# 1 \\\"`cat contains-input-name`\\\"") (bash "%{dep:../tools/cvt_emit.exe} < `cat contains-input-name`"))))))

;; srcfiles: ("asmpackager.mli" "emitaux.ml" "comballoc.ml" "cmm_invariants.mli" "branch_relaxation_intf.ml" "CSEgen.ml" "coloring.mli" "asmlink.mli" "asmpackager.ml" "dataflow.mli" "cmm_invariants.ml" "interval.mli" "cmmgen.mli" "split.ml" "reg.ml" "branch_relaxation.mli" "interf.ml" "reloadgen.ml" "reloadgen.mli" "strmatch.ml" "emit.ml" "arch.ml")
