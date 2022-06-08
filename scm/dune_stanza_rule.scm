(load "dune_actions.scm")

(define (rule->ocaml-outfile-names stanza)
  ;; (format #t "rule->ocaml-outfile-names: ~A\n" stanza)
  (let ((result
         (let* ((rule-alist (cdr stanza))
                (targets (if-let ((targets (assoc 'targets rule-alist)))
                                 (cadr targets) #f)))
           ;; (format #t "targets: ~A\n" targets)
           (if targets
               (let ((target (if (symbol? targets)
                                 (symbol->string targets)
                                 targets)))
                 ;; (format #t "targets ml?: ~A\n" targets)

                 (if (or (string-suffix? ".mli" target)
                         (string-suffix? ".ml" target))
                     target
                     '()))
               '()))))
    ;; (format #t "ocaml outfiles: ~A\n" result)
    (if (list? result)
        result
        (list result))))

(define (normalize-copy-rule pkg-path action stanza srcfiles)
  (format #t "NORMALIZE-COPY-RULE: ~A: ~A\n" pkg-path action)
  ;; (format #t "  STANZA: ~A\n" stanza)

  (let* ((src (cadr action))
         (dst (caddr action)))

    `(:copy
      (:src ,src)
      (:dst ,dst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://dune.readthedocs.io/en/stable/dune-files.html#rule
;; (rule
;;  (target[s] <filenames>)
;;  (action  <action>)
;;  <optional-fields>)

;; <optional-fields> are:
;; (deps <deps-conf list>) to specify the dependencies of the rule.
;; (mode <mode>)
;; (fallback) is deprecated and is the same as (mode fallback)
;; (locks (<lock-names>))

;; (alias <alias-name>) specify the alias this rule belongs to.
;; Building this alias means building the targets of this rule.

;; (package <package>) specify the package this rule belongs to. This
;; rule will be unavailable when installing other packages in release
;; mode.

;; (enabled_if <blang expression>) specifies the boolean condition
;; that must be true for the rule to be considered.

;; NB: devs may use 'targets' for a single output file.

;; rules always have an action? exactly one?

;; The OBazl rule we emit depends on the fields:
;;     target(s) field implies Bazel genrule
;;     action w/o target implies ocaml_test?

;; deps:
;;  (deps data.ml services.ml main.mli main.ml (:src_dir TEZOS_PROTOCOL))
;;  (deps (alias runtest_sandbox))
;; dune-specific, from tezos/src/proto_genesis/lib_protocol:
;;  (deps .tezos_protocol_genesis.objs/native/tezos_protocol_genesis.cmx)


;; run action: multiple ways to express tool:
;;   (deps    (:exe bip39_tests.exe)) (action  (run %{exe})))
;;   (action  (run %{exe:pbkdf_tests.exe})))

;; this one is just for forcing a build of the exes?
;; if so we can ignore it
;; (rule
;;  (alias buildtest)
;;  (deps bench_tool.exe bench_simple.exe)
;;  (action progn))

;; this one runs an executable dep, so we emit ocaml_test
;; (rule
;;  (alias runbench_alpha_simple)
;;  (deps bench_simple.exe)
;;  (action (chdir %{workspace_root} (run %{exe:bench_simple.exe}))))

;; write a file:
;; (rule
;;  (targets environment.ml)
;;  (action
;;   (write-file %{targets}
;;               "module Name = struct let name = \"009-PsFLoren\" end
;; include Tezos_protocol_environment.MakeV2(Name)()
;; module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
;; ")))


;; Normalization: omit the rules we don't need. The remaining rules
;; require package- (or global-) level analysis, so leave that to the
;; emitter.

;; actions:
;;  (with-stdout-to %{targets}
       ;; (chdir %{workspace_root}
       ;;   (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

;; run actions:
;; (run %{exe})
;; (run %{exe:test.exe})
;; (run %{exe:test_tezos.exe})
;; (run %{exe:../config/discover.exe} -ocamlc %{ocamlc})
;; (run %{bin:tezos-protocol-compiler} ./)

;; (rule (target[s] <filenames>) (action  <action>) <optional-fields>)
;; q: can a rule stanza have multiple actions?
(define (normalize-stanza-rule pkg-path srcfiles stanza)
  (format #t "\nNORMALIZE-STANZA-RULE: ~A\n" pkg-path)
  (format #t "rule stanza: ~A\n" stanza)
  (format #t "rule srcfiles: ~A\n" srcfiles)

  ;; for other stanza types we can normalize fields in isolation. For
  ;; 'rule' stanzas, we need a higher level of analysis, so we cannot
  ;; 'map' over the fields. Instead we extract the fields into local
  ;; vars.

  (let ((rule-alist (cdr stanza)))
    (format #t "rule-alist: ~A\n" rule-alist)
            ;; (action (assoc 'action rule-alist))
            ;; (alias (assoc 'alias rule-alist))
            ;; (deps (assoc 'deps rule-alist))
            ;; (enabled_if (assoc 'enabled_if rule-alist))
            ;; (fallback (assoc 'fallback rule-alist))
            ;; (locks (assoc 'locks rule-alist))
            ;; (mode (assoc 'mode rule-alist))
            ;; (package (assoc 'package rule-alist))
            ;; (target (assoc 'target rule-alist))
            ;; (targets (assoc 'targets rule-alist)))

            ;; (naction (if action
            ;;              (normalize-action action target targets deps))))

            ;; (format #t "rule alist: ~A\n" rule-alist)
            ;; (format #t "  action: ~A\n" action)

            ;; (format #t "  naction: ~A\n" naction)

            ;; (format #t "  alias: ~A\n" alias)
            ;; (format #t "  deps: ~A\n" deps)
            ;; (format #t "  enabled_if: ~A\n" enabled_if)
            ;; (format #t "  fallback: ~A\n" fallback)
            ;; (format #t "  locks: ~A\n" locks)
            ;; (format #t "  mode: ~A\n" mode)
            ;; (format #t "  package: ~A\n" package)
            ;; (format #t "  target: ~A\n" target)
            ;; (format #t "  targets: ~A\n" targets)
            ;; (if targets
            ;;     (if (> (length targets) 2)
            ;;         (format #t "   MULTI-TARGETS\n")))

            ;; if we have a target, then we must have an action that generates it.
            ;; the action will have ${targets}?

            ;; (error 'debug "debugging")

    ;; example: (rule (alias buildtest) (deps test_clic.exe) (action (progn)))

    (cond
     ((assoc 'action rule-alist)
      (begin
        (normalize-action
         pkg-path (assoc 'action rule-alist) stanza srcfiles)))

     ((assoc 'copy rule-alist)
      (begin
        (normalize-copy-rule
         pkg-path (assoc 'copy rule-alist) stanza srcfiles)))

     ((assoc 'copy_files# rule-alist)
      (begin
        (normalize-copy-rule
         pkg-path (assoc 'copy_files# rule-alist) stanza srcfiles)))

     (else
      (format #t "UNHANDLED RULE: ~A\n" rule-alist)))))

    ;; (let ((result (map (lambda (fld-assoc)
    ;;                      ;; (display (format #f "fld: ~A" fld-assoc)) (newline)
    ;;                      (let ((fld (if (pair? fld-assoc) fld-assoc (list fld-assoc))))


    ;;                        (case (car fld-assoc)
    ;;                          ((action)
    ;;                           (normalize-action action target targets deps))

    ;;                          (else (cons 'rule fld-assoc)))))

    ;;                    (cdr stanza))))
    ;;   ;; (cons 'rule
    ;;         result
    ;;         ;; )
    ;;   )
;; ))

  ;; (let* ((s
  ;;         (map (lambda (fld-assoc)
  ;;                (let ((fld (if (pair? fld-assoc)
  ;;                               fld-assoc
  ;;                               (list fld-assoc))))
  ;;                  (case (car fld-assoc)
  ;;                    ((target)
  ;;                     )
  ;;                    ((targets)
  ;;                     )
  ;;                    ((action)
  ;;                     ;; (action progn), (action (progn)) ???
  ;;                     ;; used to force build of deps?
  ;;                     )
  ;;                    ((alias) ;; used (always?) to force tgt build and
  ;;                     ;; thus build of deps. no other way to refer to
  ;;                     ;; the rule, no 'name' attrib.

  ;;                     ;;ignore - no need
  ;;                             ;; in bazel but - should we generate a
  ;;                             ;; Bazel alias?
  ;;                     '())
  ;;                    ((deps) ;; examples:
  ;;                     ;; (deps (:exe bip39_tests.exe))

  ;;                     ;; (deps bench_tool.exe bench_simple.exe) ;;lib_shell/bench

  ;;                     ;; (deps (universe) (:script get-git-info.mlt))

  ;;                     ;; (deps (glob_files *.ml{,i}))
  ;;                     ;; (deps (glob_files contracts/*))
  ;;                     ;; (deps (package tezos-protocol-demo-noops))

  ;;                     ;; (deps (alias runtest_requester))
  ;;                     ;; (deps (alias runtest_proto_alpha) (alias runtest_saturation_fuzzing) (alias runtest_test_tez_repr))

  ;;                     ;; pass it on, emitter must match the :exe val
  ;;                     ;; against targets in the pkg

  ;;                     )
  ;;                    ;; we'll ignore these, but pass them thru anyway
  ;;                    ((mode)    `(:mode    ,(cadr fld-assoc)))
  ;;                    ((locks)   `(:locks   ,(cadr fld-assoc)))
  ;;                    ((package) `(:package ,(cadr fld-assoc)))
  ;;                    (else
  ;;                     ))))
  ;;              (cdr stanza))))
  ;;   s)
  ;; )

;; (define (normalize-stanza-rule stanza)
;;   ;; (display (format #f "dir: ~A" pfx)) (newline)
;;   ;; (display (format #f "normalize-stanza-rule: ~A" stanza)) (newline)
;;   ;; (rule
;;   ;;  (target[s] <filenames>)
;;   ;;  (action  <action>)
;;   ;;  <optional-fields>)
;;   ;; <action> produces target <filename(s)>

;;   ;; <optional-fields>:
;;   ;; (deps <deps-conf list>)
;;   ;; (mode <mode>): standard, fallback, various promote-*
;;   ;; (fallback) is deprecated and is the same as (mode fallback)
;;   ;; (locks (<lock-names>)) run action while holding locks
;;   ;; (alias <alias-name>) Building alias = building targets of this rule.
;;   ;; (package <package>) rule will be unavailable when installing other packages in release mode.
;;   ;; (enabled_if <blang expression>) must be true for the rule to be considered.

;;   ;; short syntax:
;;   ;; (rule
;;   ;;  (target b)
;;   ;;  (deps   a)
;;   ;;  (action (copy %{deps} %{target})))
;;   ;;  => (rule (copy a b))

;;   ;; IMPORTANT: obazl ignores 'promote' stuff. we do not write to the
;;   ;; src tree, ever. However, dune rules may write to the source tree.
;;   ;; example: tezos/lib_version has a rule that runs a git cmd to get
;;   ;; version metadata and writes it to a file in the source tree.
;;   ;; probably pretty common practice. The Bazel way: using stamping,
;;   ;; and/or use a separate tool to maintain source files like this.

;;   ;; e.g. tezos/src/lib_requester/test:
;;   ;; (rule (alias runtest_requester) (action (run %{exe:test_requester.exe})))

;;   ;; tezos/src/lib_version:
;;   ;; (rule (targets generated_git_info.ml) (deps (universe) (:script get-git-info.mlt)) (action (with-stdout-to %{targets} (run %{ocaml} unix.cma %{script}))))

;;   ;; TODO: handle %{...} syntax
;;   (let ((result (map (lambda (fld-assoc)
;;                        ;; (display (format #f "fld: ~A" fld-assoc)) (newline)
;;                        (let ((fld (if (pair? fld-assoc) fld-assoc (list fld-assoc))))


;;                          (case (car fld-assoc)
;;                            ((name) (list :name
;;                                          (cons (cadr fld-assoc)
;;                                                (normalize-module-name
;;                                                 (cadr fld-assoc)))))
;;                            (else fld-assoc))))
;;                      (cdr stanza))))
;;     (cons 'rule (list result))))

