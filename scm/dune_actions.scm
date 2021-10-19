
;; https://dune.readthedocs.io/en/stable/concepts.html?highlight=progn#user-actions

;; available actions:

;; (run <prog> <args>) <prog> is resolved locally if it is available
;; in the current workspace, otherwise it is resolved using the PATH

;; (dynamic-run <prog> <args>) to execute a program that was linked against dune-action-plugin library. <prog> is resolved in the same way as in run

;; (chdir <dir> <DSL>) to change the current directory

;; (setenv <var> <value> <DSL>) to set an environment variable

;; (with-<outputs>-to <file> <DSL>) to redirect the output to a file, where <outputs> is one of: stdout, stderr or outputs (for both stdout and stderr)

;; (ignore-<outputs> <DSL>) to ignore the output, where <outputs> is one of: stdout, stderr or outputs

;; (with-stdin-from <file> <DSL>) to redirect the input from a file

;; (with-accepted-exit-codes <pred> <DSL>) specifies the list of expected exit codes for the programs executed in <DSL>. <pred> is a predicate on integer values, and is specified using the Predicate language. <DSL> can only contain nested occurrences of run, bash, system, chdir, setenv, ignore-<outputs>, with-stdin-from and with-<outputs>-to. This action is available since dune 2.0.

;; (progn <DSL>...) to execute several commands in sequence

;; (echo <string>) to output a string on stdout

;; (write-file <file> <string>) writes <string> to <file>
;;    Bazel: genrule, or custom rule using ctx.actions.write?
;;    to the extent that writing src to a file is common,
;;    an obazl rule might be in order,
;;    e.g. ocaml_string_template or some such.
;;    for now, custom tezos rules: tezos_template, tezos_file, etc.
;;    or just a simple genrule
;;    see tezos-protocol-compiler/dune_protocol.template
;; e.g.
;; (rule
;;  (targets environment.ml)
;;  (action
;;   (write-file %{targets}
;;               "... string content ...
;; ")))
;; => tezos_file(name = "envfile", out = "environment.ml",
;;               content = """...""")
;; or
;; tezos_template(name = "envfile" out = "environment.ml"
;;                template = "filename.tpl", data = "...")

;; (cat <file>) to print the contents of a file to stdout

;; (copy <src> <dst>) to copy a file

;; (copy# <src> <dst>) to copy a file and add a line directive at the beginning

;; (system <cmd>) to execute a command using the system shell: sh on Unix and cmd on Windows

;; (bash <cmd>) to execute a command using /bin/bash. This is obviously not very portable

;; (diff <file1> <file2>) is similar to (run diff <file1> <file2>) but is better and allows promotion. See Diffing and promotion for more details

;; (diff? <file1> <file2>) is similar to (diff <file1> <file2>) except that <file2> should be produced by a part of the same action rather than be a dependency, is optional and will be consumed by diff?.

;; (cmp <file1> <file2>) is similar to (run cmp <file1> <file2>) but allows promotion. See Diffing and promotion for more details

;; (no-infer <DSL>) to perform an action without inference of dependencies and targets. This is useful if you are generating dependencies in a way that Dune doesn’t know about, for instance by calling an external build system.

;; (pipe-<outputs> <DSL> <DSL> <DSL>...) to execute several actions (at least two) in sequence, filtering the <outputs> of the first command through the other command, piping the standard output of each one into the input of the next. This action is available since dune 2.7.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples from tezos:

;; (action (progn))  ;; in many places


;; src/proto_000_Ps9mPmXa/lib_protocol/dune:
;; (action
;;  (run
;;   %{libexec:tezos-protocol-compiler:replace}
;;   %{libexec:tezos-protocol-compiler:dune_protocol.template}
;;   "dune.inc.gen"
;;   %{libexec:tezos-protocol-compiler:final_protocol_versions}))
;; ...
;; (action (diff dune.inc dune.inc.gen))

;; src/proto_000_Ps9mPmXa/lib_protocol/dune.inc:
;; (action
;;   (write-file %{targets}
;;               "module Name = struct let name = \"000-Ps9mPmXa\" end
;; include Tezos_protocol_environment.MakeV0(Name)()
;; module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
;; "))
;; ...
;; (action
;;  (with-stdout-to %{targets}
;;    (chdir %{workspace_root}
;;       (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "000_Ps9mPmXa"))))


;; src/proto_003_PsddFKi3/lib_protocol/dune.inc
;; (action (with-stdout-to %{targets}
;;            (chdir %{workspace_root}
;;              (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

 ;; (action (run %{bin:tezos-protocol-compiler}  ./))

;; src/bin_proxy_server/dune
;;  (action (run %{lib:tezos-tooling:lint.sh} %{deps}))

;; src/bind_codec:
;; (rule
;;  (action
;;   (progn
;;    (write-file void_for_linking-alpha.empty "")
;;    (write-file void_for_linking-005-PsBabyM1.empty "")
;;    (write-file void_for_linking-006-PsCARTHA.empty "")
;;    (write-file void_for_linking-007-PsDELPH1.empty "")
;;    (write-file void_for_linking-008-PtEdo2Zk.empty "")
;;    (write-file void_for_linking-009-PsFLoren.empty "")
;;    (write-file void_for_linking-010-PtGRANAD.empty "")
;; )))

(define (normalize-write-file action stanza) ;; action target targets deps)
  (format #t "  Action: write-file ~A\n" action)

  ;; CAVEAT: a write-file action may have 'deps or other fields, which
  ;; are not necessarily included in the out string.

  (let* ((file (cadadr action))
         (str (cdr (cdadr action)))
         (rule-alist (cdr stanza))
         (deps (assoc 'deps rule-alist))
         (target (assoc 'target rule-alist))
         (targets (assoc 'targets rule-alist))
         ;; outfile will usually be '%{targets}' or '%{target}', but
         ;; could be a literal, or ?
         (outfile (if (equal? file '%{targets})
                      ;; for write-file, must be singleton?
                      (cadr targets)
                      (if (equal? file '%{target})
                          (cadr target)
                          (begin
                            (format #t "WARNING: write-file out: ~A\n" file)
                            file)))))

    ;;     (format #t "  File: ~A ~A\n" target targets))
    ;; (format #t "  String: ~A\n" str)

    `(:write-file
      (:out ,outfile)
      (:str ,(car str))
      ,(if deps `(:deps ,@(cdr deps)) '())
      (:raw ,stanza))))

;; expand-deps
;; (:name <dependencies>) available as %{name} in actions.
;; (file <filename>) or simply <filename>
;; (alias <alias-name>): e.g. (alias src/runtest)
;; (alias_rec <alias-name>): recursively in all children directories wherever it is defined. For instance: (alias_rec src/runtest) might depend on (alias src/runtest), (alias src/foo/bar/runtest), …
;; (glob_files <glob>)
;; (source_tree <dir>): depend on all srcs in the subtree at root <dir>
;; (universe): depend on everything in the universe; this is only for dependencies in the installed world, you must still specify all dependencies that come from the workspace.
;; (package <pkg>) depend on all files installed by <package>, as well as on the transitive package dependencies of <package>. This can be used to test a command against the files that will be installed
;; (env_var <var>): depend on the value of the environment variable <var>. If this variable becomes set, becomes unset, or changes value, the target will be rebuilt.
;; (sandbox <config>): require a particular sandboxing configuration. <config> can be one (or many) of:
;; always: the action requires a clean environment.
;; none: the action must run in the build directory.
;; preserve_file_kind: the action needs the files it reads to look like normal files (so dune won’t use symlinks for sandboxing

;; expand-deps: deps -> file-deps, vars, env-vars
(define (expand-deps deps)
  ;; extract:
  ;; 1. file deps: '(file ..), <filename>, '(glob_files ...)
  ;; 2. dep vars:  '(:name deps)
  ;; 3. '(universe) ...
  ;; 4. '(env_var ...)
  ;; 5. aliases - resolve and include with files?

  (if deps
      (let recur ((deps deps)
                  (filedeps '())
                  (vardeps '())
                  (env-vars '())
                  (universe #f)
                  (aliases '())
                  (unresolved '()))

        (if (null? deps)
            (values filedeps vardeps env-vars universe aliases unresolved)
            (if (pair? (car deps))
                (cond
                 ((equal? (car deps) '(universe))
                  (recur (cdr deps)
                         filedeps vardeps env-vars #t aliases unresolved))

                 ;; depvar
                 ((char=? #\: (string-ref (symbol->string (caar deps)) 0))
                  (recur
                   (cdr deps) filedeps (cons (car deps) vardeps)
                   env-vars universe aliases unresolved))
                 (else
                  (recur (cdr deps)
                         filedeps vardeps env-vars universe aliases
                         (cons (car deps) unresolved)))
                 )
                ;; must be a filedep
                (recur (cdr deps)
                       (cons (car deps) filedeps)
                       vardeps env-vars universe aliases unresolved))))
      (values #f #f #f #f #f #f)))

(define (normalize-cmd-dsl dsl depvars)
  (format #t "normalize-cmd-dsl: ~A\n" dsl)
  (if (null? dsl)
      '()
      (if (pair? (car dsl))
          (cond
           ((equal? 'run (caar dsl))
            (cons
             (cons :run (normalize-cmd-dsl (cdar dsl) depvars))
             (normalize-cmd-dsl (cdr dsl) depvars)))
            (else
             (cons (caar dsl) (normalize-cmd-dsl (cdr dsl) depvars))))
          (cond
           ((equal? 'chdir (car dsl)) ;; discard
            (normalize-cmd-dsl (cddr dsl) depvars))
           (else
            (cons (car dsl) (normalize-cmd-dsl (cdr dsl) depvars)))))))

;; (with-<outputs>-to <file> <DSL>), <outputs>= stdout | stderr | outputs
(define (normalize-with-stdout-to action stanza) ;;target targets deps)
  (format #t "  Action: with-stdout-to ~A\n" action)

  ;; FIXME: if 'targets' is a list the following won't work
  (let* ((file (cadadr action))
         (dsl (cadr (cdadr action)))
         ;; dsl may contain embedded actions, e.g. 'chdir', 'setenv', etc.
         (rule-alist (cdr stanza))
         (target (assoc 'target rule-alist))
         (targets (assoc 'targets rule-alist))
         (outfile (if (equal? file '%{targets})
                      (cadr targets)
                      (if (equal? file '%{target})
                          (cadr target)
                          (begin
                            (format #t "WARNING: write-file out: ~A\n" file)
                            file)))))
    (let-values (((filedeps depvars env-vars universe aliases unresolved)
                  (expand-deps (assoc 'deps rule-alist))))
      (format #t "filedeps: ~A\n" filedeps)
      (format #t "depvars: ~A\n" depvars)
      (format #t "env-vars: ~A\n" env-vars)
      (format #t "universe: ~A\n" universe)
      (format #t "aliases: ~A\n" aliases)
      (format #t "unresolved: ~A\n" unresolved)

      (let ((cmd (normalize-cmd-dsl dsl depvars)))
        (format #t "cmd: ~A\n" cmd)

    ;; (format #t "DSL: ~A\n" dsl)

    `(:with-stdout-to
      (:out ,outfile)
      (:cmd ,cmd)
      (:depvars ,depvars)
      ,(if filedeps `(:filedeps ,@(cdr filedeps)) '())
      (:raw ,stanza))))))

  ;; (let ((result
  ;;        (let recur ((tokens (cdadr action)))
  ;;          (if (null? tokens)
  ;;              '()
  ;;              (cons (car tokens) (recur (cdr tokens)))))))
  ;;   (format #t "  Result: ~A\n" result)
  ;;   result))

;; (run <prog> <args>) as primary action of rule stanza
;; 'run' may be embedded in other stanzas, e.g.
;; (with-stdout-to %{targets} (chdir %{workspace_root} (run ...)))

;; vars:
;; lib:<public-library-name>:<file> expands to the installation path of the file <file> in the library <public-library-name>. If <public-library-name> is available in the current workspace, the local file will be used, otherwise the one from the installed world will be used.
;; e.g.
  ;; (run
  ;;   %{libexec:tezos-protocol-compiler:replace}
  ;;   %{libexec:tezos-protocol-compiler:dune_protocol.template}
  ;;   "dune.inc.gen"
  ;;   %{libexec:tezos-protocol-compiler:final_protocol_versions})))

(define (normalize-run-stanza action stanza)
  (format #t "  Action: run ~A\n" action)
  (format #t "  stanza: ~A\n" stanza)

  ;; (let* ((prog (cadadr action))
  ;;        (rule-alist (cdr stanza))

  ;;        (deps (assoc 'deps rule-alist))
  ;;        ;; 'deps' may contain var definitions used in cmd string
  ;;        ;; e.g. (:src_dir TEZOS_PROTOCOL)
  ;;        (depvars (expand-deps deps))

  ;;        (dsl (cadr (cdadr action)))
  ;;        ;; dsl may contain embedded actions, e.g. 'chdir', 'setenv', etc.
  ;;        (cmd (normalize-cmd-dsl dsl depvars))
  ;;        (target (assoc 'target rule-alist))
  ;;        (targets (assoc 'targets rule-alist))
  ;;        (outfile (if (equal? file '%{targets})
  ;;                     (cadr targets)
  ;;                     (if (equal? file '%{target})
  ;;                         (cadr target)
  ;;                         (begin
  ;;                           (format #t "WARNING: write-file out: ~A\n" file)
  ;;                           file)))))

  ;;   (if (not (null? depvars))
  ;;       (format #t "DEPS VARS: ~A\n" depvars))

  ;;   (format #t "DSL: ~A\n" dsl)

  ;;   `(:genrule
  ;;     (:out ,outfile)
  ;;     (:cmd ,cmd)
  ;;     (:depvars ,depvars)
  ;;     ,(if deps `(:deps ,@(cdr deps)) '())
  ;;     (:raw ,stanza)))
  stanza)

(define (normalize-progn action stanza)
  ;; tezos: (action progn), (action (progn)), (action (progn (...) ...))
  ;; missing but maybe possible: (action progn ...)

  ;; examples:
  ;; (rule (alias runtest)
  ;;       (package tezos-protocol-004-Pt24m4xi)
  ;;       (deps (alias runtest_sandbox))
  ;;       (action (progn)))
  ;; "Building this alias means building the targets of
  ;; this rule." Presumably that means deps too.
  ;; "The typical use of the alias stanza is to define tests:
  ;; (rule (alias   runtest)
  ;;       (action (run %{exe:my-test-program.exe} blah)))

  ;; "[T]o define a test a pair of alias and
  ;; executable stanzas are required."

  (cond
   ((eq? 'progn (cadr action)) ;; (action progn)
    (format #t "    progn: simple\n")
    stanza)

   ((equal? '(progn) (cadr action)) ;; (action (progn)) - null action?
    (format #t "  Action: empty progn\n")
    ;; this is a null action. used with alias, to force build of deps?
    ;; usually alias is 'runtest'?
    ;; but dune "aliases" like runtest are group names, not true aliases.
    ;; so there is no corresponding Bazel target.

    ;; e.g.
    ;; (rule (alias runtest)
    ;;       (package tezos-protocol-004-Pt24m4xi)
    ;;       (deps (alias runtest_sandbox))
    ;;       (action (progn)))

    ;; verify we also have an alias:
    (let* ((rule-alist (cdr stanza))
           (alias (assoc 'alias rule-alist)))
      ;; aliases used by tezos: runtest, buildtest
      (if alias
          (format #t "    progn alias: ~A\n" alias)
          (format #t "    progn w/o alias: ~A\n" stanza)))
    stanza)

   (else ;; (action (progn ...))
    (format #t "    progn: compound\n")
    stanza)))

(define (normalize-action action stanza) ;; action target targets deps)
  (format #t "  Normalizing action: ~A\n" action)

  (let ((key (if (pair? (cadr action))
                 ;; e.g. (action (run ...)), (action (progn))
                 (caadr action)
                 ;; e.g. (action progn)
                 (cadr action))))
    (format #t "  action key: ~A\n" key)
    (case key  ;; (car action)
      ((run) (normalize-run-stanza action stanza))

      ((progn) (normalize-progn action stanza))

      ((with-stdout-to) (normalize-with-stdout-to action stanza))

      ((with-stderr-to) stanza)
      ((with-stdin-from) stanza)
      ((with-outputs-to) stanza)

      ((write-file) (normalize-write-file action stanza))

      (else stanza))))
