(load "dune_stanza_library.scm")
(load "dune_stanza_rule.scm")
(load "utils.scm")

(define (normalize-dune-stanza pkg-path dune-project-stanzas srcfiles stanza)
  ;; stanza: (<stanza type> (flda ...) (fldb ...) ...)
  ;; (newline)
  ;; (format #t "raw: ~A\n" stanza)
  ;; (if (equal? "src/lib_client_base" (cadr (assoc :pkg-path (cdr stanza))))
  ;;     (format #t "normalize-dune-stanza: ~A" srcfiles))
  ;; (format #t "normalize-dune-stanza stanza: ~A\n" stanza)
  ;; (format #t "normalize-dune-stanza srcs: ~A\n" srcfiles)
  ;; (if (null? srcfiles)
  ;;     (format #t "pkg: ~A\n" pkg-path))

  (let* ((ocaml-srcs (if (null? srcfiles)
                         '()
                         (if-let ((srcs (assoc :ocaml (cadr srcfiles))))
                                 (cadr srcs) '())))
         (s (case (car stanza)
              ((alias) (normalize-stanza-alias stanza))
              ((copy_files) (normalize-stanza-copy_files stanza))
              ((data_only_dirs) (normalize-stanza-data_only_dirs stanza))
              ((env) (normalize-stanza-env stanza))
              ((executable) (normalize-stanza-executable
                             pkg-path ocaml-srcs stanza))

              ((executables) (normalize-stanza-executables
                              pkg-path ocaml-srcs stanza))

              ((install) (normalize-stanza-install
                          pkg-path dune-project-stanzas stanza))

              ((library) (normalize-stanza-library
                          pkg-path ocaml-srcs stanza))

              ((ocamllex) (normalize-stanza-ocamllex stanza))
              ((rule) (normalize-stanza-rule
                          pkg-path ocaml-srcs stanza))
              ((test) (normalize-stanza-test ocaml-srcs stanza))
              ((tests) (normalize-stanza-tests ocaml-srcs stanza))

              ((:dune-project) stanza)

              (else
               (format #t "normalize-dune-stanza unhandled: ~A\n" stanza)))))

    ;; update global public -> private name table
    (case (car stanza)
      ((executables)
       (begin))
      ((library)
       (begin
          (let* ((private-name (assoc-in '(:name :private) (cadr s)))
                 (public-name  (assoc      :public_name   (cadr s))))
            (if (and private-name public-name)
                (begin
                  ;; (format #t "writing ~A => ~A\n" private-name public-name)
                  (hash-table-set! private-name->public-name
                                   (cadr private-name)
                                   (cadr public-name)))))
          ))
      )

    ;; return normalized stanza
    s))

(define (normalize-stanza-copy_files stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-copy_files: ~A" stanza)) (newline)
  ;; (copy_files
  ;;  <optional-fields>
  ;;  (files <glob>))
  ;; <optional-fields> are:
  ;; (alias <alias-name>) to specify an alias to which to attach the targets.
  ;; (mode <mode>) to specify how to handle the targets
  ;; (enabled_if <blang expression>)

  ;; The short form (copy_files <glob>)
  ;; is equivalent to (copy_files (files <glob>))

  ;; TODO: parse glob
  ;; glob: https://dune.readthedocs.io/en/stable/concepts.html#glob
  ;; glob :: path/pattern, path is optional

  ;; e.g.
  ;; (copy_files bindings/{rustzcash_ctypes_c_stubs.c,rustzcash_ctypes_stubs.ml,rustzcash_ctypes_bindings.ml})

  (let ((result (if (pair? (cadr stanza))
                    stanza
                    (list (car stanza)
                          (list (list 'files (cdr stanza)))))))
    ;; (display (format #f "norm result: ~A" result)) (newline)
    result)
  )

(define (normalize-stanza-ocamllex stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-ocamllex: ~A" stanza)) (newline)
  ;; (ocamllex <names>) is essentially a shorthand for:
  ;; (rule
  ;;  (target <name>.ml)
  ;;  (deps   <name>.mll)
  ;;  (action (chdir %{workspace_root}
  ;;           (run %{bin:ocamllex} -q -o %{target} %{deps}))))
  ;; To use a different rule mode, use the long form:
  ;; (ocamllex
  ;;  (modules <names>)
  ;;  (mode    <mode>))
  ;; normalized: (ocamllex (modules <names>))

  ;; e.g. (ocamllex point_parser)
  (let ((result (if (pair? (cadr stanza))
                    stanza
                    (list (car stanza) (cdr stanza)))))
    ;; (display (format #f "norm result: ~A" result)) (newline)
    (list 'ocamllex (list result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (normalize-stanza-alias stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-alias: ~A" stanza)) (newline)
  (cons 'alias (list (cdr stanza))))

(define (normalize-stanza-data_only_dirs stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-env: ~A" stanza)) (newline)
  (cons 'data_only_dirs
        (list (list (cons :dirs (cdr stanza))))))

(define (normalize-stanza-env stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-env: ~A" stanza)) (newline)
  (cons 'env (list (cdr stanza))))

(define (expand-modules-fld modules srcfiles)
  ;; modules:: (modules Test_tezos)
  ;; (format #t "  expand-modules-fld: ~A\n" modules)
  ;; see also normalize-stanza-lib-fld-modules in dune_stanza_fields.scm
  (let* ((modules (cdr modules)))
    (if (null? modules)
        (values '() '())
        ;; (let ((result
        (let recur ((modules modules)
                    (direct '())
                    (indirect '()))
          ;; (format #t "ms: ~A; direct: ~A\n" modules direct)
          (cond
           ((null? modules)
            (values direct indirect))

           ((equal? :standard (car modules))
            (let ((newseq (srcs->module-names srcfiles direct)))
              ;; (format #t "modules :STANDARD ~A\n" newseq)
              ;; (format #t "CDRMODS ~A\n" (cdr modules))
              (recur (cdr modules) (append newseq direct) indirect)))
           ;; (concatenate direct
           ;;              (norm-std-modules (cdr modules))))
           ((pair? (car modules))
            (let-values (((exp gen)
                          (recur (car modules) '() '())))
              (recur (cdr modules)
                     (concatenate exp direct)
                     (concatenate gen indirect))))

           ((indirect-module-dep? (car modules) srcfiles)
            (begin
              ;; (format #t "INDIRECT: ~A\n" (car modules))
              (recur (cdr modules)
                     direct (cons (car modules) indirect))))

           (else
            (recur (cdr modules)
                   (cons (car modules) direct)
                   indirect))))
        ;;      ))
        ;; ;;(format #t "RESULT: ~A\n" result)
        ;; (reverse result))
        ))
  )

(define (update-public-exe-table pkg-path pubname filename)
  (format #t "update-public-exe-table: ~A => ~A/~A\n"
          pubname pkg-path filename)
  (let* ((pubname (symbol->string pubname))
         (target-label (string-append "//" pkg-path ":"
                                      (symbol->string filename))))

    (hash-table-set! public-exe->label
                     (string->symbol pubname) target-label)
    (hash-table-set! public-exe->label
                     (string->symbol (string-append pubname ".exe"))
                     target-label)
    (hash-table-set! public-exe->label
                     (string->symbol (string-append pubname ".bc"))
                     target-label)

    (let recur ((path-segs (reverse (string-split pkg-path #\/)))
                (pfx ""))
      (format #t "path-segs: ~A\n" path-segs)
      (if (null? path-segs)
          '()
          (let* ((pfx (string-append (car path-segs) "/" pfx))
                 (k (string-append pfx pubname)))
            (format #t " k: ~A;  pfx:   ~A\n" k pfx)
            (format #t " target label: ~A\n" target-label)
            (hash-table-set! public-exe->label
                             (string->symbol k) target-label)
            (hash-table-set! public-exe->label
                             (string->symbol
                              (string-append k ".exe"))
                             target-label)
            (hash-table-set! public-exe->label
                             (string->symbol
                              (string-append k ".bc"))
                             target-label)
            (recur (cdr path-segs) pfx))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contingent dep example:
;; (select void_for_linking-alpha-protocol-plugin from
;;   (tezos-protocol-plugin-alpha -> void_for_linking-alpha-protocol-plugin.empty)
;;   (-> void_for_linking-alpha-protocol-plugin.empty))

(define (normalize-lib-select select)
  (format #t "normalize-lib-select: ~A\n" select)
  (let ((select-file (cadr select))
        (clauses (cdddr select)))
    ;; if clause RHSs are equal, then reason for select is to force
    ;; link of LHS lib

    ;; LHS lib is also the select condition. we map it to a config
    ;; setting, so the result is: if config-setting then add LHS lib
    ;; to deps. so the user passes config settings, selecting modules
    ;; to link - build profiles (platforms)

    ;; e.g.
    ;; platform(
    ;; name = "std_protocols",
    ;; constraint_values = [
    ;;     ":proto_a",
    ;;     ":proto_b",
    ;;     ":proto_c",
    ;; ])
    ;; then a config_setting with these constraing values
    ;; then select on config_setting

    (format #t "SELECT file: ~A\n" select-file)
    (format #t "CLAUSES: ~A\n" clauses)
    select))

(define (normalize-exe-libraries libs-assoc stanza-alist)
  (let-values (((constant contingent)
                (let recur ((libs libs-assoc)
                            (constants '())
                            (contingents '()))
                  (if (null? libs)
                      (values constants contingents)
                      (if (list? (car libs))
                          (recur (cdr libs)
                                 constants
                                 (cons
                                  (if (equal? 'select (caar libs))
                                      (normalize-lib-select (car libs))
                                      (car libs))
                                  contingents))
                          (recur (cdr libs)
                                 (cons (car libs) constants)
                                 contingents))))))
    (format #t "constant deps: ~A\n" constant)
    (format #t "contingent deps: ~A\n" contingent)
    `(:deps
      ((:constant ,constant)
       (:contingent ,contingent)))))

(define (normalize-executable privname pubname stanza-alist)
  (format #t "normalize-executable ~A, ~A\n" privname pubname)
  ;; NB: stanza-alist excludes 'names' and 'public_names'
  ;; if has modules list, one must match 'name'
  (let* ((modules (assoc 'modules stanza-alist)))
    (if modules
        (if (> (length (cdr modules)) 1)
            (begin
              (format #t "    WARNING: multiple modules for executable\n"))
            ;; just like the 'modules' fld of 'library' stanzas
            (if (not
                 (or (equal? (cadr privname) (cadr modules))
                     (equal? (normalize-module-name (cadr privname))
                             (normalize-module-name (cadr modules)))))
                ;; error?
                (format #t "    WARNING: name/module mismatch: ~A : ~A\n"
                        (cadr privname)
                        (normalize-module-name (cadr modules))))))

    (list :executable  ;; (car stanza)
          (concatenate
           `((:name ((:private ,privname)
                     (:public ,pubname))))
           (map (lambda (fld-assoc)
                 ;; (format #t "exec fld-assoc: ~A\n" fld-assoc)
                 ;; (let ((fld (if (pair? fld-assoc)
                 ;;                fld-assoc
                 ;;                (list fld-assoc))))
                 (case (car fld-assoc)
                   ((libraries)
                    (normalize-exe-libraries fld-assoc stanza-alist))
                   ((modules)
                    (let-values (((direct indirect)
                                  (expand-modules-fld modules srcfiles)))
                      ;; (format #t
                      ;;         "    direct: ~A\n    indirect ~A\n"
                      ;;         direct indirect)
                      (let* ((raw `((:raw ,fld-assoc)))
                             (direct (if (null? direct)
                                         raw
                                         (cons `(:direct ,@direct) raw)))
                             (indirect (if (null? indirect)
                                           direct
                                           (cons `(:indirect
                                                   ,@(reverse indirect))
                                                 direct)))
                             (result `(:modules ,indirect)))
                        ;;(format #t "RESULT ~A: ~A\n" stanza-name result)
                        result)))
                   ((flags) (normalize-stanza-fld-flags fld-assoc))
                   ((foreign_stubs)
                    (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

                   (else fld-assoc)))
               stanza-alist)))))

(define (normalize-stanza-executable pkg-path srcfiles stanza)
  ;; (format #t "normalize-stanza-executable: ~A\n  ~A\n" pkg-path stanza)

  ;; "<name> is a module name that contains the main entry point of
  ;; the executable." So 'name' must correspond to a .ml file. If
  ;; 'modules' is present, then is must include (explicitly or
  ;; implicitly) the 'name' module. OBazl will pass this 'name' module
  ;; in the 'main' attribute.

  ;; "There can be additional modules in the current directory, you
  ;; only need to specify the entry point."

  ;; Yet OCaml has no concept of "main entry point", so there is no
  ;; way to single out one module among several as 'main'. Top-level
  ;; code will simply be executed in link order. An intended main
  ;; entry point could be preceded by modules that do some kind of
  ;; initialization (e.g. read a file or socket and create a data
  ;; structure that main uses), and/or followed by modules that do
  ;; some kind of post-processing. Dune does not seem to accomodate
  ;; such structuring; presumably "entry point" means "last in link
  ;; order.  (Which is how OBazl treats the 'main' attribute.)

  ;; The modules are there to be linked into the executable; it does
  ;; not follow that there are any inter-deps among them. In
  ;; particular, they need not be deps of the main module.

  ;; The 'libraries' and 'flags' fields apply to all modules.

  ;; So to normalize an 'executable' stanza: expand the 'modules'
  ;; and 'libraries' fields; normalize 'name' and 'flags'.

  (let* ((stanza-alist (cdr stanza))
         ;; 'name' fld is required
         (privname (assoc 'name stanza-alist))
         ;; private_name?
         (modules (assoc 'modules stanza-alist)))
    ;; (format #t " N: ~A\n" privname)
    ;; (format #t " Ms: ~A\n" modules)

    ;; if has modules list, one must match 'name'
    (if modules
        (if (> (length (cdr modules)) 1)
            (begin
              (format #t "    WARNING: multiple modules for executable\n"))
              ;; just like the 'modules' fld of 'library' stanzas
            (if (not
                 (or (equal? (cadr privname) (cadr modules))
                     (equal? (normalize-module-name (cadr privname))
                             (normalize-module-name (cadr modules)))))
                ;; error?
                (format #t "    WARNING: name/module mismatch: ~A : ~A\n"
                        (cadr privname)
                        (normalize-module-name (cadr modules))))))

    (list :executable  ;; (car stanza)
          (map (lambda (fld-assoc)
                 ;; (format #t "exec fld-assoc: ~A\n" fld-assoc)
                 ;; (let ((fld (if (pair? fld-assoc)
                 ;;                fld-assoc
                 ;;                (list fld-assoc))))
                 (case (car fld-assoc)
                   ((name)
                    ;; (format #t "exec name: ~A\n" (cadr name))
                    (let ((pubname (assoc 'public_name stanza-alist)))
                      (if pubname
                          (begin
                            `(:name ((:private ,(cadr privname))
                                     (:public ,(cadr pubname))))
                            (update-public-exe-table pkg-path
                                                     (cadr pubname)
                                                     (cadr pubname))
                            (update-public-exe-table pkg-path
                                                     (cadr privname)
                                                     (cadr pubname)))
                          (begin
                            `(:name ((:private ,(cadr privname))))
                            (update-public-exe-table pkg-path (cadr privname)
                                                     (cadr privname)))
                          ))
                    )

                   ((public_name) '())
                   ((libraries)
                    ;; ???
                    )
                   ((modules)
                    (let-values (((direct indirect)
                                  (expand-modules-fld modules srcfiles)))
                      ;; (format #t
                      ;;         "    direct: ~A\n    indirect ~A\n"
                      ;;         direct indirect)
                      (let* ((raw `((:raw ,fld-assoc)))
                             (direct (if (null? direct)
                                         raw
                                         (cons `(:direct ,@direct) raw)))
                             (indirect (if (null? indirect)
                                           direct
                                           (cons `(:indirect
                                                   ,@(reverse indirect))
                                                 direct)))
                             (result `(:modules ,indirect)))
                        ;;(format #t "RESULT ~A: ~A\n" stanza-name result)
                        result)))
                   ((flags) (normalize-stanza-fld-flags fld-assoc))
                   ((foreign_stubs)
                    (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

                   (else fld-assoc)))
               stanza-alist))))

;; ((executables ((names test_clic) (libraries tezos-clic alcotest-lwt) (flags (:standard -open Tezos_stdlib -open Tezos_clic)))) (rule ((alias buildtest) (deps test_clic.exe) (action (progn)))) (rule ((alias runtest_clic) (action (run %{exe:test_clic.exe})))) (rule ((alias runtest) (package tezos-clic) (deps (alias runtest_clic)) (action (progn)))))

;; EXES normalized: (:executable ((:name ((:private main_snoop) (:module Main_snoop))) (public_names tezos-snoop) (package tezos-snoop) (libraries tezos-base tezos-base.unix tezos-stdlib-unix tezos-clic tezos-benchmark tezos-benchmark-examples tezos-shell-benchmarks tezos-benchmarks-proto-alpha str ocamlgraph pyml pyml-plot latex) (:opts ((:standard) (:opens ("Tezos_benchmark" "Tezos_stdlib_unix" "Tezos_base")) (:raw (:standard -open Tezos_base__TzPervasives -open Tezos_stdlib_unix -open Tezos_benchmark -linkall)) (:flags -open Tezos_base__TzPervasives -open Tezos_stdlib_unix -open Tezos_benchmark -linkall)))))
;; normalize-stanza-executables: src/lib_store/test

(define (normalize-stanza-executables pkg-path srcfiles stanza)
  (format #t "normalize-stanza-executables: ~A\n" pkg-path) ;; ~A\n" stanza)
  (format #t "  stanza:  ~A\n" stanza)
  ;; (:executables (names test_clic) ...
  (let* ((stanza-alist (cdr stanza))
         (privnames (assoc 'names stanza-alist))
         (pubnames (assoc 'public_names stanza-alist))
         (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist))
         )
         ;; (names-ct (length names))
         ;; (typ (if (> names-ct 1) :executables :executable)))

    (format #t "stanza-alist: ~A\n" stanza-alist)
    (format #t "filtered-stanza-alist: ~A\n" filtered-stanza-alist)

    (format #t "    names: ~A\n" privnames)
    ;; only five cases in tezos of:
    (format #t "    pubnames: ~A\n" pubnames)

    (if (> (length (cdr privnames)) 1)
        (begin
          (format #t "MULTIPLE NAMES\n")
          (if pubnames
              (if (not (equal?
                        (length (cdr privnames))
                        (length (cdr pubnames))))
                  (error
                   'bad-arg "names and public_names differ in length")))))

    ;; we add the pub/priv names to the lookup table, so that the
    ;; emitter can resolve references to them.
    ;; dune emits *.exe and *.bc, which may be referred to
    ;; in dunefiles, so we add them to the lookup table too.

    (if pubnames
        (map (lambda (privname pubname)
               ;; (format #t "exe-name ~A\n" exe-name)
               (update-public-exe-table pkg-path pubname privname)
               (update-public-exe-table pkg-path privname privname)
               (normalize-executable privname pubname filtered-stanza-alist)
               ;; `(:executable ((:name ((:private ,privname)
               ;;                        (:public ,pubname)))))
               )
             (cdr privnames) (cdr pubnames))

        ;; else no pubnames
        (map (lambda (privname)
               (format #t "exe-name ~A\n" privname)
               (update-public-exe-table pkg-path privname privname)
               `(:executable ((:name ((:private ,privname)))))
                        ;; (:module ,(normalize-module-name name))))))
               )
             (cdr privnames)))))

    ;; (list typ
    ;;       (map (lambda (fld-assoc)
    ;;              ;; (format #t "exec fld-assoc: ~A\n" fld-assoc)
    ;;              ;; (let ((fld (if (pair? fld-assoc)
    ;;              ;;                fld-assoc
    ;;              ;;                (list fld-assoc))))
    ;;              (case (car fld-assoc)
    ;;                ;; each name in names corresponds to a module (src file)
    ;;                ((names)
    ;;                 (begin
    ;;                   (format #t "  names: ~A\n" fld-assoc)
    ;;                   (normalize-stanza-fld-names (cdr fld-assoc))))
    ;;                ((modules
    ;;                  (format #t "  modules: ~A\n" fld-assoc)))
    ;;                ((public_name)
    ;;                 (normalize-stanza-fld-public_name (cadr fld-assoc)))
    ;;                ((flags) (normalize-stanza-fld-flags fld-assoc))
    ;;                ((foreign_stubs)
    ;;                 (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))
    ;;                (else fld-assoc)))
    ;;            (cdr stanza)))
    ;; ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (install
;;  (files hello.txt) ;; required field?
;;  (section share)  ;; required field?
;;  (package mypackage)) ;; optional field
;;
;; in src/proto_demo_noops/lib_protocol/dune:
;; (install
;;  (section lib)
;;  (package tezos-protocol-demo-noops)
;;  (files (TEZOS_PROTOCOL as raw/TEZOS_PROTOCOL)))
;; mapping: %{lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL}
;;          => //src/proto_demo_noops/lib_protocol:TEZOS_PROTOCOL

(define (update-installation-table src dst)
  (hash-table-set! installation-table src dst))

;; (files (tezos-init-sandboxed-client.sh as tezos-init-sandboxed-client.sh))
;; (files (replace.exe as replace)
;;        dune_protocol dune_protocol.template final_protocol_versions)
;; (files foo)
;; (files (foo as a) (bar as b))
;; i.e. (files (file-name as link-name))
;; so we return a list of pairs (link-name file-name)
(define (normalize-install-files files)
  (format #t "normalize-install-files: ~A\n" files)
  (let recur ((entries files)
              (links '())
              (files '()))
    (if (null? entries)
        (values links files)
        (if (list? (car entries))
            (if (equal? 3 (length (car entries)))
                (begin
                  (format #t "LEN3: ~A\n" (car entries))
                (if (equal 'as (cadr (car entries)))
                    (begin
                      (format #t "caar: ~A\n" (caar entries))
                      (recur (cdr entries)
                             (cons (caddr (car entries)) links)
                             (cons (caar entries) files)))
                    ;; else list of links == files
                    (recur (cdr entries)
                           (append (car entries) links)
                           (append (car entries) files))))
                ;; else list != 3, list of links == files
                (recur (cdr entries)
                           (append (car entries) links)
                           (append (car entries) files)))
            ;; else not a list
            (recur (cdr entries)
                   (cons (car entries) links)
                   (cons (car entries) files))))))

(define (normalize-stanza-install pkg-path dune-project-stanzas stanza)
  (format #t "normalize-stanza-install: ~A\n  ~A\n" pkg-path stanza)
  (format #t "    dune-project: ~A\n" dune-project-stanzas)
  (let* ((stanza-alist (cdr stanza))
         (section (cadr (assoc 'section stanza-alist)))
         (package (if-let ((pkg (assoc 'package stanza-alist)))
                          (cadr pkg)
                          (if-let ((pkg-name
                                    (assoc 'name dune-project-stanzas)))
                                  (cadr pkg-name)
                                  #f))))
    (let-values (((link-names file-names)
                  (normalize-install-files
                   (cdr (assoc 'files stanza-alist)))))
      (format #t "link-names: ~A\n" link-names)
      (format #t "file-names: ~A\n" file-names)

      (for-each (lambda (link-name file-name)
                  (update-installation-table
                   (string->symbol
                    (string-append (symbol->string section)
                                   ":" (symbol->string package)
                                   ":" (symbol->string link-name)))
                   (string->symbol
                    (string-append "//" pkg-path
                                   ":" (symbol->string file-name)))))
                link-names file-names)

      (format #t "dune pkg name: ~A\n" package)
      (cons :install (list (cdr stanza))))))

(define (normalize-stanza-test srcfiles stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-test: ~A" stanza)) (newline)
  (list (car stanza)
        (map (lambda (fld-assoc)
               (let ((fld (if (pair? fld-assoc) fld-assoc (list fld-assoc))))
                 (case (car fld-assoc)
                   ((name) (normalize-stanza-fld-name (cadr fld-assoc)))
                   ((flags) (normalize-stanza-fld-flags (cadr fld-assoc)))
                   ((foreign_stubs)
                    (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))
                   (else fld-assoc))))
             (cdr stanza))))

(define (normalize-stanza-tests srcfiles stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-tests: ~A" stanza)) (newline)
  (cons 'tests (list (cdr stanza))))
