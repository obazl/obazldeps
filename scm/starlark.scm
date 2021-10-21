;; (format #t "loading @camlark//starlark.scm\n")

(load "dune.scm")
(load "opam.scm")
(load "string.scm")
(load "utils.scm")

;; (load "stuff.scm") ;; concatenate

(define (stanza->alist stanza)
  (if (alist? stanza)
      stanza
      (if (pair? stanza)
          '()
          (error 'bad-type "cannot map stanza to alist"))))

(define (name->opts-sym name)
  (string-append (string-upcase (symbol->string name))
                 "_OPTS"))

(define (name->deps-sym name)
  (string-append (string-upcase (symbol->string name))
                 "_DEPS"))

(define (starlark-emit-build-file-hdr outp dune-pkg-tbl)
  (format outp "load(\"@obazl_rules_ocaml//ocaml:rules.bzl\",\n")

  (if (or (assoc-in '(:stanzas :executable) (cdr dune-pkg-tbl))
          (assoc-in '(:stanzas :executables) (cdr dune-pkg-tbl)))
      (format outp "    \"ocaml_executable\",\n"))

  (if (pkg-has-library? dune-pkg-tbl)
      (format outp "    \"ocaml_library\",\n"))

  (if (or (assoc-in '(:stanzas library) (cdr dune-pkg-tbl))
          (assoc-in '(:stanzas :executable) (cdr dune-pkg-tbl))
          (assoc-in '(:stanzas :executables) (cdr dune-pkg-tbl)))
      (format outp "    \"ocaml_module\",\n"))

  (if (pkg-has-ns-archive? dune-pkg-tbl)
      (format outp "    \"ocaml_ns_archive\",\n"))

  (if (assoc-in '(:stanzas signature) (cdr dune-pkg-tbl))
      (format outp "    \"ocaml_signature\",\n"))

  (format outp ")\n")

  (newline outp)
  ;; (format outp "#############################\n")

  '())

(define (standard-flags)
  '(-O2))

(define (resolve-stdlib module)
  ;; (format #t "resolve-stdlib: ~A\n" module)
  (let ((std (stdlib-tbl module)))
    ;; (format #t " resolve?: ~A\n" opam)
    std))

(define (stanza-deps fs-path modname stanza-alist)
  ;; (format #t "stanza-deps: ~A//~A\n" fs-path modname)
  ;; (format #t "    stanza: ~A\n" stanza-alist)
  ;; (if (equal? modname 'tezos-protocol-environment-sigs)
  ;;     (format #t "~A stanza-deps: ~A\n" modname stanza-alist))
  ;; NB: resolved deps use (always?) public_name

  ;; (:deps ((:constant (...)) (:contingent (select clauses...))))
  (if-let ((deps (assoc-in '(:deps :constant) stanza-alist)))
          (begin
            ;; (format #t "stanza-deps resolved: ~A\n" deps)
            (map (lambda (dep)
                   ;; (format #t "%%%% dep: ~A %%%%\n" dep)
                   ;; ;; (format #t "priv->pub: ~A => ~A\n"
                   ;; ;;         dep (private-name->public-name dep))
                   ;; (format #t "pub->mod: ~A => ~A\n"
                   ;;         dep (public-name->module-name dep))
                   ;; (format #t "mapping ~A: ~A\n"
                   ;;         dep (public-name->module-name
                   ;;                    (private-name->public-name dep)))

                   ;; lookup: first names tbl, then opam, then stdlib

                   (if-let ((namerec (names-tbl dep)))
                                      ;; (private-name->public-name dep))))
                           (begin
                             ;;(format #t "PUBNAME: ~A\n" pubname)
                             ;; (format #t "NAMEREC: ~A\n" namerec)
                             (cadr (assoc :label namerec))

                             ;; (string-append (cadr (assoc :label namerec)) ":" (symbol->string dep))

                             ;; (if-let ((modspec (modules-tbl modname)))
                             ;;         (string-append
                             ;;          "//" (cadr (assoc :path modspec))
                             ;;          ":" (symbol->string dep))
                             ;;         ;; ":" (symbol->string pname))
                             ;;         (string-append "NO MODSPEC: "
                             ;;                        (symbol->string dep)))
                             )

                           (if-let ((namerec (names-tbl ;; public-name->module-name
                                      (private-name->public-name dep))))
                                      ;; (private-name->public-name dep))))
                                   (cadr (assoc :label namerec))

                                   ;; (string-append (cadr (assoc :label namerec)) ":" (symbol->string dep))
                                   ;; (begin
                                   ;;   ;;(format #t "PUBNAME: ~A\n" pubname)
                                   ;;   ;; (format #t "PNAMEREC: ~A\n" namerec)
                                   ;;   (if-let ((modspec (modules-tbl namerec)))
                                   ;;           (string-append
                                   ;;            (if (string=?
                                   ;;                 fs-path
                                   ;;                 (cadr (assoc :path modspec)))
                                   ;;                ":"
                                   ;;                (string-append
                                   ;;                 "//" (cadr (assoc :path modspec))))
                                   ;;            (symbol->string
                                   ;;             (private-name->public-name dep))
                                   ;;            )
                                   ;;           ;; ":" (symbol->string pname))
                                   ;;           (string-append "NO MODSPEC: "
                                   ;;                          (symbol->string dep))))

                                   (if-let ((opam (resolve-opam dep)))
                                           opam
                                           ;; no opam
                                           (if-let ((stdlib (resolve-stdlib dep)))
                                                   stdlib
                                                   (format #t "UNRESOLVED: ~A: ~A\n"
                                                           namerec dep))))))
                 (cadr deps)))
          '()))

(define (stanza-opts stanza-alist)
  ;; (format #t "STANZA-OPTS: ~A\n" stanza-alist)
  (let ((flags (assoc-in '(:opts :flags) stanza-alist))
        (opens (assoc-in '(:opts :opens) stanza-alist)))
    (values flags opens)))

(define (expand-modules-list modules)
  ;; (format #t "expand-modules-list: ~A\n" modules)
  (if-let ((sublist (member :standard modules)))
      (begin
        ;; (format #t "SUBLIST ~A\n" sublist)
        sublist)
      (begin
        (format #t "HUH?\n")
        modules)))

;; returns list
(define (stanza-modules libname stanza-alist)
  ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
  ;; (if (equal? libname 'Tezos_base)
  ;;       (format #t "stanza-submodules ~A: ~A\n" libname stanza-alist))
  (let ((modules-direct (if-let (directs (assoc-in
                                          '(:modules :direct) stanza-alist))
                                (cdr directs) '()))
        (modules-indirect (if-let (indirects (assoc-in
                                       '(:modules :indirect) stanza-alist))
                                  (cdr indirects) '())))
    ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
    ;; (if (equal? libname 'Tezos_base)
        ;; (begin
        ;;   (format #t "BASE direct subms: ~A\n" modules-direct)
        ;;   (format #t "BASE indirect subms: ~A\n" modules-indirect))
    (concatenate modules-direct modules-indirect)))

;; (submodules (stanza-submodules modname stanza-alist)))
(define (stanza-submodules modname stanza-alist)
  ;; (format #t "stanza-submodules: ~A: ~A\n" modname stanza-alist)
  ;; :direct modules: src files
  ;; :indirect modules: generated
  (let ((modules-direct (if-let (directs (assoc-in
                                          '(:submodules :direct) stanza-alist))
                                (cdr directs) '()))
        (modules-indirect (if-let (indirects (assoc-in
                                       '(:submodules :indirect) stanza-alist))
                                  (cdr indirects) '())))
    ;; (if (equal? modname 'Tezos_protocol_environment_sigs)
    ;; (if (equal? modname 'Tezos_base)
        ;; (begin
        ;;   (format #t "BASE direct subms: ~A\n" modules-direct)
        ;;   (format #t "BASE indirect subms: ~A\n" modules-indirect))
    (concatenate modules-direct modules-indirect)))

(define (module->executable-deps _stanzas module)
  ;; (format #t "module->executable-deps: ~A\n" module)
  ;; (if (equal? module 'Registerer)
  ;;     (format #t " Registerer stanzas: ~A\n" _stanzas))
  (let ((result
         (let recur ((stanzas _stanzas))
           ;; (if (not (null? stanzas))
           ;;     (format #t " car stanzas: ~A\n" (car stanzas)))
           (if (null? stanzas)
               #f
               (if (equal? :executable (caar stanzas))
                   (let* ((stanza-alist (cadr (car stanzas)))
                          ;; (_ (format #t "xSTANZA-ALIST: ~A\n" stanza-alist))
                          ;; first get all 'modules', so we can check if ours is included
                          (modules-main
                           (if-let ((ms
                                     (assoc-in
                                      '(:modules :main) stanza-alist)))
                                   (cdr ms) '()))
                          (modules-direct
                           (if-let ((ms
                                     (assoc-in
                                      '(:modules :direct) stanza-alist)))
                                   (cdr ms) '()))
                          (modules-indirect
                           (if-let ((ms
                                     (assoc-in
                                      '(:modules :indirect) stanza-alist)))
                                   (cdr ms) '()))
                          (modules (concatenate
                                    modules-main modules-direct modules-indirect))
                          )
                     ;; (if (equal? module 'Main_byte)
                     ;;     (format #t "Main_byte MODULES: ~A\n" modules))
                     (if modules
                         (begin
                           (if (member module modules)
                               (let* ((constants
                                       (if-let ((ms
                                                 (assoc-in
                                                  '(:deps :constant) stanza-alist)))
                                               (cadr ms) '()))
                                      (contingents
                                       (if-let ((ms
                                                 (assoc-in
                                                  '(:deps :contingents) stanza-alist)))
                                               (cadr ms) '()))
                                      (deps (concatenate constants contingents)))
                                 deps)
                               (recur (cdr stanzas))))
                         ;; our module not in exec's module list
                         ;; recur??
                         #f))
                   ;; not and executable stanza, skip
                   (recur (cdr stanzas)))
               ))))
    result))

(define (explicit-ns? modname pubname)
  (if (equal? modname pubname)
      #f
      (let ((s1 (if (symbol? modname) (symbol->string modname) modname))
            (s2 (if (symbol? pubname) (symbol->string pubname) pubname)))
        (not (and (string=? (substring s1 1) (substring s2 1))
                  (char=? (char-upcase (string-ref s1 0))
                          (char-upcase (string-ref s2 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WE SHOULD NOT HAVE 'executables' - all should be normalized to list
;; of 'executable'
(define (starlark-emit-executables outp fs-path stanza)
  ;; (format #t "starlark-emit-executables ~A\n" stanza)
  (error 'unexpected-stanza "ERROR: unexpected 'executables' stanza")

  (for-each (lambda (exe)
              (format #t "EXE: ~A\n" exe))
            (cadr (assoc :names stanza)))

  ;; (let* (;; (libname (cdadr (assoc :name stanza)))
  ;;       (modname (cadr (assoc-in '(:name :module) stanza)))
  ;;       (pubname (if-let ((pn (assoc ':public_name stanza)))
  ;;                        (cadr pn) #f))
  ;;       (opts (stanza-opts stanza))
  ;;       (deps (stanza-deps fs-path pubname stanza))
  ;;       (submodules (stanza-submodules modname stanza))
  ;;       )

  ;;   (format #t "SUBMs: ~A\n" submodules)
  ;;   ;; (format outp "~A = [\n" (name->opts-sym modname))
  ;;   ;; (for-each (lambda (opt)
  ;;   ;;             (format outp "    \"~A\",\n" opt)
  ;;   ;;             )
  ;;   ;;           opts)
  ;;   ;; (format outp "]\n")
  ;;   ;; (newline outp)

  ;;   ;; (format outp "~A = [\n" (name->deps-sym modname))
  ;;   ;; (for-each (lambda (dep)
  ;;   ;;             (format outp "    \"~A\",\n" dep)
  ;;   ;;           )
  ;;   ;;           deps)
  ;;   ;; (format outp "]\n")
  ;;   ;; (newline outp)

  ;;   (begin
  ;;     (format outp "#################\n")
  ;;     (format outp "ocaml_executable(\n")
  ;;     (format outp "    name    = \"~A\",\n" pubname)
  ;;     (format outp "    modules = [\n")
  ;;     (for-each (lambda (mod)
  ;;                 (format outp "        \":~A\",\n"
  ;;                         (symbol->string
  ;;                          (normalize-module-name mod))
  ;;                         ))
  ;;               submodules)
  ;;     ;; (for-each (lambda (mod)
  ;;     ;;             (format outp "        \"~A\",\n" mod))
  ;;     ;;           modules)
  ;;     (format outp "    ],")
  ;;     (format outp ")\n\n")
  ;;     ;;(format outp "#############################\n")
  ;;     ))
  )

(define (starlark-emit-executable-target outp fs-path stanza-alist)
  ;; (format #t "starlark-emit-executable-target ~A :: ~A\n"
  ;;         fs-path (cadr (assoc-in '(:name :private) stanza-alist)))
  (let* ((privname (cadr (assoc-in '(:name :private) stanza-alist)))
         (mainname (normalize-module-name privname))
         (pubname (if-let ((pn (assoc-in '(:name :public) stanza-alist)))
                          (cadr pn)
                          privname))
         ;; (pubname (if-let ((pn (assoc :public_name stanza-alist)))
         ;;                  (cadr pn)
         ;;                  privname))
         (tgtname (string-append (symbol->string pubname) ".out"))
         (exename privname)
         (deps (stanza-deps fs-path pubname stanza-alist))
         (submodules (stanza-submodules mainname stanza-alist))
         (modules (stanza-modules mainname stanza-alist))
         )
    (let-values (((flags opens) (stanza-opts stanza-alist)))
      ;; (format #t "TARGET: ~A\n" tgtname)
      ;; (format #t "MAIN: ~A\n" mainname)
      ;; (format #t "FLAGS: ~A\n" flags)
      ;; (format #t "OPENS: ~A\n" opens)
      ;; (format #t "DEPS: ~A\n" deps)
      ;; (format #t "MODULES: ~A\n" modules)
      ;; (format #t "SUBMs: ~A\n" submodules)

      (format outp "~A = [\n" (name->opts-sym pubname))
      ;; (for-each (lambda (opt)
      ;;             (format outp "    \"~A\",\n" opt)
      ;;             )
      ;;           opts)
      (format outp "]\n")
      (newline outp)

      (format outp "~A = [\n" (name->deps-sym pubname))
      (for-each (lambda (dep)
                  (format outp "    \"~A\",\n" dep)
                )
                deps)
      (format outp "]\n")
      (newline outp)

      (begin
        (format outp "#################\n")
        (format outp "ocaml_executable(\n")
        (format outp "    name    = \"~A\",\n" tgtname)
        ;; attr 'exe': string name of outfile excluding extension,
        ;; not a dependency
        (format outp "    exe     = \"~A\",\n" exename)
        (format outp "    main    = \":~A\",\n" mainname)

        (if (not (null? modules))
            (begin
              (format outp "    deps = [\n")
              (for-each (lambda (mod)
                          (if (not (equal? mod mainname))
                              (format outp "        \":~A\",\n"
                                      (symbol->string
                                       (normalize-module-name mod))
                                      )))
                        modules)
              ;; (for-each (lambda (mod)
              ;;             (format outp "        \"~A\",\n" mod))
              ;;           modules)
              (format outp "    ],\n")))

        (format outp ")\n")
        (newline outp)
        ;;(format outp "#############################\n")
        )))
  )

(define (starlark-emit-executable-targets outp fs-path stanzas)
  ;; (format #t "starlark-emit-executable-targets ~A\n" ;; : ~A\n"
  ;;         fs-path)
  ;; (format #t "stanzas: ~A\n" stanzas)
  (format outp "##############################\n")
  (format outp "####  Executable Targets  ####\n")
  (for-each (lambda (stanza)
              ;; (format #t "stanza x: ~A ~A\n" fs-path (car stanza))
              (case (car stanza)
                ((:executable)
                 (starlark-emit-executable-target
                  outp fs-path (cadr stanza)))

                ((:executables)
                 (starlark-emit-executables
                  outp fs-path stanza))
                (else ;; ignore others
                 ;; (format outp "UNCAUGHT: ~A\n" stanza)
                 )))
            stanzas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (starlark-emit-aggregate-target outp typ fs-path stanza)
  ;; (format #t "STARLARK-EMIT-AGGREGATE-TARGET ~A\n" fs-path)
  ;; (format #t "    stanza: ~A\n" stanza)
  (let* ((stanza-alist (cadr stanza))
         (privname (cadr (assoc-in '(:name :private) stanza-alist)))
         (modname (normalize-module-name privname))
         (pubname (cadr (assoc-in '(:name :public) stanza-alist)))
         (explicit-ns (explicit-ns? modname pubname))
         (deps (stanza-deps fs-path pubname stanza-alist))
         (submodules (stanza-submodules modname stanza-alist)))
    (let-values (((flags opens) (stanza-opts stanza-alist)))
      ;; (format #t "FLAGS: ~A\n" flags)
      ;; (format #t "OPENS: ~A\n" opens)
      ;; (format #t "DEPS: ~A\n" deps)
      ;; (format #t "SUBMs: ~A\n" submodules)

      (format outp "~A = [\n" (name->opts-sym modname))
      (if flags
          (for-each (lambda (opt)
                      (format outp "    \"~A\",\n" opt))
                (cadr flags)))
      (if opens
          (for-each (lambda (open)
                      (format outp "    \"-open\", \"~A\",\n" open))
                (cadr opens)))
      (format outp "]\n")
      (newline outp)

      (format outp "~A = [\n" (name->deps-sym modname))
      (for-each (lambda (dep)
                  (format outp "    \"~A\",\n" dep)
                  )
                deps)
      (format outp "]\n")
      (newline outp)

      (case typ
        ((library)
         (begin
           (format outp "##############\n")
           (format outp "ocaml_library(\n")
           (format outp "    name    = \"~A\",\n" pubname)
           (format outp "    modules = [\n")
           (for-each (lambda (mod)
                       (format outp "        \":~A\",\n"
                               (symbol->string
                                (normalize-module-name mod))
                               ))
                     submodules)
           ;; (for-each (lambda (mod)
           ;;             (format outp "        \"~A\",\n" mod))
           ;;           modules)
           (format outp "    ],")
           (format outp ")\n\n")))
        ;; (format outp "#############################\n")))
        ((ns-archive)
         (begin
           (format outp "#################\n")
           (format outp "ocaml_ns_archive(\n")
           (format outp "    name       = \"~A\",\n" pubname)
           (if explicit-ns
               (format outp "    ns         = \"~A\",\n" modname))
           (format outp "    submodules = [\n")
           (for-each (lambda (mod)
                       (format outp "        \":~A\",\n"
                               (symbol->string
                                (normalize-module-name mod))
                               ))
                     submodules)
           (format outp "    ],")
           (format outp ")\n\n")))
        ;; (format outp "#############################\n")))
        (else
         (format outp "UNCAUGHT X\n\n")))
      ))
  )

(define (starlark-emit-stanza-deps-and-flags outp typ stanza)
  (format #t "starlark-emit-stanza-deps-and-flags\n")
  ;; (format #t "    stanza: ~A\n" stanza)
  (let ((modname (cadr (assoc-in '(:name :module) stanza)))
        (deps (stanza-deps modname stanza))
        (submodules (stanza-submodules modname typ stanza)))
    (let-values (((flags opens) (stanza-opts stanza)))
      (format #t "FLAGS: ~A\n" flags)
      (format #t "OPENS: ~A\n" opens)
      (format outp "~A = [\n" (name->opts-sym modname))
      (if flags
          (for-each (lambda (opt)
                      (format outp "    \"~A\",\n" opt))
                (cadr flags)))
      (if opens
          (for-each (lambda (open)
                      (format outp "    \"-open\", \"~A\",\n" open))
                (cadr opens)))
      ;; (for-each (lambda (opt)
      ;;             (format outp "    \"~A\",\n" opt)
      ;;             )
      ;;           opts)
      (format outp "]\n")
      (newline outp)

      (format outp "~A = [\n" (name->deps-sym modname))
      (for-each (lambda (dep)
                  (format outp "    \"~A\",\n" dep))
                deps)
      (format outp "]\n")
      (newline outp)
      )))

(define (library-wrapped? stanza)
  (if-let ((wrapped (assoc 'wrapped (cadr stanza))))
          (if (equal? 'false (cadr wrapped))
              #f
              #t)
          #t))

(define (starlark-emit-aggregate-targets outp fs-path stanzas)
  ;; (format #t "starlark-emit-aggregate-targets ~A\n" fs-path)

  ;; only emit header if aggregators
  (let recur ((stanzas stanzas))
    (if (null? stanzas)
                  '()
                  (if (equal? (car stanzas) :library)
                      (begin
                        (format outp "#############################\n")
                        (format outp "####  Aggregate Targets  ####\n"))
                      (recur (cdr stanzas)))))

  (for-each (lambda (stanza)
              (case (car stanza)
                ((:library)
                 ;; FIXME: if no sources, then it's a placeholder for
                 ;; install, so skip? e.g. tezos/src/tooling

                 (starlark-emit-aggregate-target
                  outp
                  (if (library-wrapped? stanza) 'ns-archive :library)
                  fs-path
                  stanza)

                 ;; (if (library-wrapped? stanza)
                 ;;     (starlark-emit-aggregate-target outp 'ns-archive
                 ;;                                     (cadr stanza))
                 ;;     ;; unwrapped dune libs do not get an aggregate rule,
                 ;;     ;; but they do have flags and deps that apply to their
                 ;;     ;; modules. so we need to emit those syms:
                 ;;     (starlark-emit-stanza-deps-and-flags outp 'ns-archive
                 ;;                                          (cadr stanza)))


                 )))
                ;; (else (format outp "UNCAUGHT: ~A\n"
                ;;               stanza))))
            stanzas))

;; (define (modules-list-contains-module? modules module)
;;   (display (format #f "modules-list-contains-module? ~A ~A"
;;                    modules module)) (newline)
;;   (any (lambda (m)
;;          (member module modules))
;;        modules)
;;   )

(define (aggregate-stanza? stanza)
  ;; (format #t "aggregate-stanza? ~A\n" (car stanza))
  ;; dune treats executables as aggregators, since they have a 'modules'
  ;; field
  (if (equal? (car stanza) :library)
      #t
      (if (equal? (car stanza) :executable)
          #t
          #f)))

;; (define (module-name->aggregate-module-name _stanzas module)
;;   ;; (format #t "module-name->aggregate-module-name: ~A\n" module)
;;   ;; (if (equal? module 'Replace)
;;   ;;     (format #t " Registerer stanzas: ~A\n" _stanzas))
;;   (let ((result
;;          (let recur ((stanzas _stanzas))
;;            (if (not (null? stanzas))
;;                (format #t " CAR stanzas: ~A\n"
;;                        (aggregate-stanza? (car stanzas))))
;;            (if (null? stanzas)
;;                #f
;;                (if (aggregate-stanza? (car stanzas))
;;                    (let* ((_ (format #t "XXXX ~A\n" (car stanzas)))
;;                           (stanza-alist (cadr (car stanzas)))

;;                           ;; FIXME: modules must include direct, indirect, and main (for executables)
;;                           ;; :submodules for aggregators,
;;                           ;; :modules for executables
;;                           (modules (if-let ((ms (assoc-in
;;                                                  '(:submodules :direct) stanza-alist)))
;;                                            (cdr ms)
;;                                            (if-let ((ms (assoc-in
;;                                                          '(:modules :direct) stanza-alist)))
;;                                                    (cdr ms)
;;                                                    (if-let ((ms (assoc-in
;;                                                                  '(:modules :main) stanza-alist)))
;;                                                            (cdr ms)
;;                                                            #f)))))
;;                      ;; (if (equal? module 'Registerer)
;;                      ;;     (format #t "Registerer MODULES: ~A\n" modules))
;;                      (if modules
;;                          (begin
;;                            (if (member module modules)
;;                                (if-let ((nm (assoc-in '(:name :public) stanza-alist)))
;;                                        (cadr nm)
;;                                        ;; executables may not have a public name?
;;                                        (if-let ((nm (assoc-in '(:name :private) stanza-alist)))
;;                                                (cadr nm)
;;                                                (error 'bad-arg
;;                                                       (format #f "module ~A not listed in modules enum ~A\n"
;;                                                               module modules))))
;;                                (recur (cdr stanzas))))
;;                          (if (equal? :library (car stanza-alist))
;;                              (let ((pubname
;;                                     (assoc-in '(:name :pubname) stanza-alist)))
;;                                (cadr pubname))
;;                              ;; no modules => no aggregation
;;                              #f)))
;;                    (recur (cdr stanzas)))))))
;;     result))

(define (stanza-alist->modules stanza-alist)
  (let* ((direct (if-let ((ms (assoc-in '(:submodules :direct) stanza-alist)))
                         (cdr ms) '()))
         (indirect (if-let ((ms (assoc-in '(:modules :direct) stanza-alist)))
                           (cdr ms) '()))
         (main (if-let ((ms (assoc-in '(:modules :main) stanza-alist)))
                       (cdr ms) '())))
    (concatenate main direct indirect)))

;; return aggregator stanza containing module
(define (module-name->aggregator module _stanzas)
  ;; (format #t "module-name->aggregator: ~A\n" module)
  ;; (if (equal? module 'Replace)
  ;;     (format #t " Registerer stanzas: ~A\n" _stanzas))
  (let ((result
         (let recur ((stanzas _stanzas))
           ;; (if (not (null? stanzas))
           ;;     (format #t " car stanzas: ~A\n"
           ;;             (aggregate-stanza? (car stanzas))))
           (if (null? stanzas)
               #f
               (if (aggregate-stanza? (car stanzas))
                   (let* ((stanza-alist (cadr (car stanzas)))
                          ;; FIXME: modules must include direct,
                          ;; indirect, and main (for executables)

                          ;; :submodules for aggregators,
                          ;; :modules for executables
                          (modules (stanza-alist->modules stanza-alist))
                          )
                     ;; (format #t "modules: ~A\n" modules)
                     (if modules
                         (begin
                           (if (member module modules)
                               (if-let ((nm (assoc-in '(:name :public) stanza-alist)))
                                       ;; return whole stanza-alist
                                       stanza-alist ;; (cadr nm)
                                       (if-let ((nm (assoc-in '(:name :private) stanza-alist)))
                                               ;; return whole stanza-alist
                                               stanza-alist
                                               ;;(cadr nm)
                                               (error 'bad-arg
                                                      (format #f "module ~A not listed in modules enum ~A\n"
                                                              module modules))))
                               (recur (cdr stanzas))))
                         (begin
                           (if (equal? :library (car stanza-alist))
                               (let ((pubname
                                      (assoc-in
                                       '(:name :pubname) stanza-alist)))
                                 (cadr pubname))
                               ;; no modules => no aggregation
                               #f))))
                   (recur (cdr stanzas)))))))
    result))

(define (module-name->opens-list _stanzas module)
  ;; 1. get stanza for module
  ;; 2. get -open flags for module
  (let ((result
         (let recur ((stanzas _stanzas))
           ;; (if (not (null? stanzas))
           ;;     (format #t " car stanzas: ~A\n" (car stanzas)))
           (if (null? stanzas)
               #f
               (if (not (member (caar stanzas) '(library executable)))
                   (recur (cdr stanzas)) ;; rule stanzas not yet normalized
                   (let* ((stanza (cadr (car stanzas))))
                     (let ((modules (if-let
                                     ((ms (assoc-in
                                           '(:modules :direct) stanza)))
                                       (cadr ms)
                                       #f)))
                     ;; (if (equal? module 'Shell_context)
                     ;;     (format #t "MODULES: ~A\n" modules))
                       (if modules
                           (begin
                             (if (member module modules)
                                 (let ((opens (assoc-in '(:opts :opens) stanza)))
                                   opens)
                                 (recur (cdr stanzas))))
                           ;; should not happen
                           (if (equal? :library (car stanza))
                               (let ((nm (assoc-in '(:name :module) stanza)))
                                 (cadr nm))
                               ;; recur??
                               #f)))))))))
    result))

;; (define (xxxresolve-libdep ns module-name dep stanzas)
;;   ;; (format #t "resolve-libdep ~A[~A]: ~A\n" ns module-name dep)
;;   ;; e.g. tezos-node[Node_config_file]: Block_hash
;;   ;; search 'libraries' of tezos-node stanza for Block_hash

;;   ;; module-name in ns depends on dep; search ns 'libraries' fld
;;   ;; NB: libraries fld uses public_names
;;   ;; example: Tezos_proxy[Light] depends on RPC_context
;;   ;; (modules-tbl 'Tezos_proxy) => :deps list contains tezos-rpc
;;   ;; (public-names-lookup 'tezos-rpc) => Tezos_rpc
;;   ;; (modules-tbl 'Tezos_rpc) => :submodules assoc list contains RPC_context
;;   ;; result: Light depends on Tezos_rpc
;;   (let* ((modspec (modules-tbl ns)) ;; e.g. Tezos_proxy
;;          (libdeps (assoc :deps modspec))) ;; :deps of Tezos_proxy
;;     ;; (format #t "modspec: ~A\n" modspec)
;;     ;; (format #t "libdeps: ~A\n" libdeps)
;;     ;; (if libdeps
;;     ;;     (format #t "  NS LIBDEPS: ~A:: ~A\n" ns (cadr libdeps)))
;;     (if libdeps
;;         (let recur ((libdeps (cadr libdeps))) ;; libdeps=list of public_names
;;           (if (null? libdeps)
;;               #f
;;               (begin
;;                 ;; (format #t "LIBDEP: ~A\n" (car libdeps))
;;                 (let ((libdep-module (private-name->public-name
;;                                       (car libdeps))))
;;                   ;; e.g. tezos-rpc => Tezos_rpc
;;                   (if-let ((libdep-mtbl (modules-tbl libdep-module)))
;;                           (if-let ((libdep-submods
;;                                     (assoc :submodules libdep-mtbl)))
;;                                   (begin
;;                                     ;; (format #t "lipdep submods: ~A:: ~A\n"
;;                                     ;;         libdep-module libdep-submods)
;;                                     (if (member (car dep) libdep-submods)
;;                                         (begin
;;                                           ;; (format #t "HIT: ~A[~A] :: ~A\n"
;;                                           ;;         ns (car dep) libdep-module)
;;                                           libdep-module)
;;                                         ;; dep not in submods of this libdep
;;                                         (recur (cdr libdeps))))
;;                                   ;; :submodules no found in libdep mtbl
;;                                   (recur (cdr libdeps)))
;;                           ;; FIXME not in modules-tbl - error?
;;                           (recur (cdr libdeps)))))))
;;         ;; ns not in modules-tbl - error?
;;         #f)))

(define (resolve-libdep ns module-name dep)
  (if (equal? 'Block_header dep)
      (format #t "resolve-libdep ~A[~A]: ~A\n" ns module-name dep))
  ;; e.g. tezos-node[Node_config_file]: Block_hash
  ;; search 'libraries' of tezos-node stanza for Block_hash

  ;; module-name in ns depends on dep; search ns 'libraries' fld
  ;; NB: libraries fld uses public_names
  ;; example: Tezos_proxy[Light] depends on RPC_context
  ;; (modules-tbl 'Tezos_proxy) => :deps list contains tezos-rpc
  ;; (public-names-lookup 'tezos-rpc) => Tezos_rpc
  ;; (modules-tbl 'Tezos_rpc) => :submodules assoc list contains RPC_context
  ;; result: Light depends on Tezos_rpc

  (let ((result
         (if-let ((modspec (modules-tbl dep))) ;; e.g. Block_header
                 (begin
                   (format #t "modspec: ~A\n" modspec)
                   (if-let ((label (assoc-in '(:label :ns) modspec)))
                           (begin
                             (format #t "ns LABEL: ~A\n" label)
                             (cadr label))
                           (if-let ((label (assoc-in
                                            '(:label :module) modspec)))
                                   (begin
                                     (format #t "module LABEL: ~A\n" label)
                                     (cadr label))
                                  #f)))
                   #f)))
    (format #t "resolved: ~A => ~A\n" dep result)
    result))

;; FIXME: what if we have a singleton .mli file?
(define (in-srcfiles? module srcfiles)
  ;; (format #t "in-srcfiles? ~A :: ~A\n"
  ;;         module (if (null? srcfiles) "()" (car srcfiles)))
;; FIXME: lookup in modules-tbl instead?
  (if (null? srcfiles)
      (begin
        ;; (format #t "not in srcfiles\n")
        #f)
      (if (equal? (normalize-module-name module)
                  (file-name->module-name (car srcfiles)))
          (string-append ":"
                         (symbol->string (normalize-module-name module)))
          (in-srcfiles? module (cdr srcfiles)))))

(define (resolve-dep ns module-name dep stanzas srcfiles)
  (format #t "resolve-dep: ~A[~A]:: ~A\n" ns module-name dep)
  ;; FIXME: handle compound deps, e,g. (Tezos_client_base Client_context)

  ;; resolution algorithm:
  ;; 1. check if dep is in same directory (colon dep)
  ;; 2. search '-open' deps
  ;; 3. search 'libraries' deps
  ;; 4. search opam deps
  ;; 5. search stdlib deps
  ;; if not found, assume its in one of the opam deps

  (let ((opens (module-name->opens-list stanzas module-name)))
    ;; (format #t "  opens: ~A\n" opens)
    (if (equal? ns (car dep))
        (string-append ":" (symbol->string (cadr dep)))

        ;; else matches a src file?
        (if-let ((label (in-srcfiles? (car dep) srcfiles)))
                label

                ;; else matches a module in an -open ns?
                (if opens
                    (let recur ((opens (cadr opens)))
                      ;; (format #t "recur ~A\n" opens)
                      (if (null? opens)
                          ;; not resolved by opened ns; try 'libraries' deps
                          (if-let ((libdep
                                    (resolve-libdep ns module-name dep)))
                                  libdep
                                  (if-let ((opam (resolve-opam dep)))
                                          opam
                                          #f))
                          ;; else try next opened dep
                          (if-let ((module-alist (modules-tbl (car opens))))
                                  (if-let ((submods (assoc :submodules module-alist)))
                                          ;; (list opens (car dep) submods)
                                          (if-let ((hit (member
                                                         (car dep)
                                                         (cdr submods))))
                                                  (begin
                                                    (car opens))
                                                  (recur (cdr opens)))
                                          (recur (cdr opens)))
                                  ;; not in this opened module, try the next
                                  (recur (cdr opens)))))
                    ;; no -open flags - try libdeps, then opam
                    (begin
                      (if-let ((libdep
                                (resolve-libdep ns module-name
                                                (car dep))))
                              libdep
                              (if-let ((opam (resolve-opam (car dep))))
                                      opam
                                      #f)))
                    )))))

(define (starlark-emit-file-targets outp fs-path stanzas dune-pkg)
  ;; (format #t "starlark-emit-file-targets: ~A\n" fs-path)

  (let ((srcfiles (if-let ((srcs (assoc-in '(:srcfiles :ocaml) dune-pkg)))
                      (sort! (cadr srcs) string<?)
                      '())))
    (if srcfiles
        (begin
          (format outp "#############################\n")
          (format outp "####  Singleton Targets  ####\n")
          (newline outp)))

    (for-each (lambda (srcfile)
                ;; (format #t "SRCFILE: ~A\n" srcfile)
                (let-values (((typ mname)
                              (if (string-suffix? ".ml" srcfile)
                                  (values :ml
                                          ;;(string-drop-right srcfile 3)
                                          (file-name->module-name srcfile))
                                  (if (string-suffix? ".mli" srcfile)
                                      (values :mli
                                              ;; (string-drop-right srcfile 4)
                                              (file-name->module-name srcfile)
                                              )
                                      (error 'bad-filename
                                             (string-append
                                              "extension should be .ml or .mli: "
                                              srcfile))))))
                  (let* ((aggregator (module-name->aggregator mname stanzas))
                         (aggregate-module-name
                          ;;(module-name->aggregate-module-name stanzas mname)
                          (if aggregator
                              (cadr (assoc-in '(:name :public) aggregator))
                              #f)))
                    ;; (format #t "aggregate-module-name: ~A\n"
                    ;;         aggregate-module-name)
                    (if (string-suffix? ".ml" srcfile)
                        (begin
                          (format outp "ocaml_module(\n")
                          (format outp "    name   = \"~A\",\n" mname)
                          (format outp "    struct = \"~A\",\n" srcfile)
                          (if (member (string-append
                                       (symbol->string mname) "i")
                                      srcfiles)
                              (format outp "    sig    = \":~A\",\n"
                                      (string-append
                                       (symbol->string mname) "_cmi")))
                          )

                        (if (string-suffix? ".mli" srcfile)
                            (begin
                              (format outp "ocaml_signature(\n")
                              (format outp "    name = \"~A_cmi\",\n" mname)
                              (format outp "    src  = \"~A\",\n" srcfile))
                            (error 'bad-filename
                                   (string-append
                                    "extension should be .ml or .mli: "
                                    srcfile))))

                    ;; (starlark-emit-build-opts outp mname stanzas dune-pkg)
                    (if aggregate-module-name
                        (format outp "    opts   = ~A,\n"
                                (name->opts-sym aggregate-module-name))
                        (if-let ((exe-deps
                                  (module->executable-deps stanzas mname)))
                                (begin
                                  (format outp "    opts   = [\n")
                                  (for-each (lambda (dep)
                                              (format outp
                                                      "        \"~A\",\n" dep))
                                            exe-deps)
                                  (format outp "    ],\n"))

                                ;; else: neither submodule nor exec dep
                                ;; => no opts?
                                ))

                    (begin
                      ;; (format #t "    procesing deps\n")
                      (if aggregate-module-name
                          (format outp "    deps   = ~A + [\n" (name->deps-sym aggregate-module-name))
                          (format outp "    deps   = [\n"))

                      (let* ((deps (codept-srcfile->depslist
                                    (string-append fs-path "/" srcfile)
                                    codept-sexp))
                             (deps (if (null? deps) '() deps)))
                        ;; (format #t "emit ~A deps: ~A\n" srcfile deps)
                        (for-each (lambda (dep)
                                    ;; (format #t "next dep: ~A\n" dep)
                                    (let ((lbl (resolve-dep
                                                aggregate-module-name
                                                mname dep stanzas
                                                srcfiles)))
                                      (format #t "LBL: ~A\n" lbl)
                                      (if (equal? lbl #<unspecified>)
                                          (format outp "       ## ~A\n" dep)
                                          (format outp "       \"~A\", ## ~A\n" lbl dep))))
                                  deps))
                      (format outp "    ]\n"))
                    (format outp ")\n\n"))))
              srcfiles)
    ))

    ;; (let ((lib-stanzas (assoc+ :library stanzas)))
    ;;   (if (not (null? lib-stanzas))
    ;;       (for-each (lambda (stanza)
    ;;                   (newline outp)
    ;;                   (format outp "lib stanza: ~A\n"
    ;;                           (assoc 'name (cdr stanza)))
    ;;                   (let ((modules (assoc 'modules (cdr stanza)))
    ;;                         (flags (if-let
    ;;                                 ((flags (assoc 'flags (cdr stanza))))
    ;;                                 (cadr flags)
    ;;                                 '()))
    ;;                         (ocamlopt-flags
    ;;                          (if-let
    ;;                           ((flags (assoc 'ocamlopt_flags (cdr stanza))))
    ;;                           (cadr ocamlopt-flags)
    ;;                           '())))
    ;;                     (format outp "modules: ~A\n" modules)
    ;;                     (format outp "flags: ~A\n" flags)
    ;;                     (format outp "ocamlopt_flags: ~A\n" ocamlopt-flags)
    ;;                     ))
    ;;                 ;;(emit-lib-args fs-path stanza srcfiles out-port))
    ;;                 lib-stanzas))
    ;;   )
    ;; ))

(define (starlark-emit-rule-target outp stanza)
  ;; (format #t "starlark-emit-rule-target: ~A\n" stanza)
  ;; (let ((libname (cdadr (assoc :name stanza)))
  ;;       (opts (stanza-opts stanza))
  ;;       (deps '("test-dep1" "test-dep2"))
  ;;       (modules '("test-mod1" "test-mod2"))
  ;;       (submodules (stanza-submodules typ stanza)))

    (format outp "################  rule  ################\n")
    (if (list? stanza)
        (begin
          (format outp "## (\n")
          (for-each (lambda (sexp)
                      (format outp "##   ~A\n" sexp))
                    stanza)
          (format outp "## )\n"))))

    ;; (format outp "genrule(\n")
    ;; (format outp "    name    = \"~A\",\n" "rulename")
    ;; (format outp "    srcs = [],\n")
    ;; (format outp "    outs = [],\n")
    ;; (format outp "    cmd  = \"\",\n")
    ;; (format outp "    tools = []\n")
    ;; (format outp ")\n\n"))

(define (starlark-emit-genrule-target outp stanza)
  ;; (format #t "starlark-emit-genrule-target: ~A\n" stanza)
  ;; (let ((libname (cdadr (assoc :name stanza)))
  ;;       (opts (stanza-opts stanza))
  ;;       (deps '("test-dep1" "test-dep2"))
  ;;       (modules '("test-mod1" "test-mod2"))
  ;;       (submodules (stanza-submodules typ stanza)))

    (format outp "########\n")
    (format outp "genrule(\n")
    (format outp "    name = \"~A\",\n" "tbd")
    (format outp "    srcs  = [\n")
    (format outp "    ],\n")
    (format outp "    outs  = [\n")
    (format outp "    ],\n")
    (format outp "    cmd  = \"~A\"\n" "tbd")
    (format outp ")\n")

    (if (list? stanza)
        (begin
          (format outp "## (\n")
          (for-each (lambda (sexp)
                      (format outp "##   ~A\n" sexp))
                    stanza)
          (format outp "## )\n")))
)

(define (starlark-emit-with-stdout-to-target outp stanza)
  ;; (format #t "starlark-emit-with-stdout-to-target: ~A\n" stanza)
  ;; (let ((libname (cdadr (assoc :name stanza)))
  ;;       (opts (stanza-opts stanza))
  ;;       (deps '("test-dep1" "test-dep2"))
  ;;       (modules '("test-mod1" "test-mod2"))
  ;;       (submodules (stanza-submodules typ stanza)))

    (format outp "########\n")
    (format outp "genrule(\n")
    (format outp "    name = \"~A\",\n"
          (string-append "gen_"
                         (symbol->string (cadr (assoc :out stanza)))))
    (format outp "    srcs  = [\n")
    (for-each (lambda (out)
                (if (list? out)
                    (for-each (lambda (outout)
                                (format outp "        \"~A\",\n" outout))
                              (cdr out) ;; FIXME: is out always (:foo a b c)?
                              )
                    (format outp "\"~A\"" out)))
              ;; FIXME: how do we know which depvars are inputs?
              (cadr (assoc :depvars stanza)))
    (format outp "    ],\n")
    (format outp "    outs  = [\n")
    (format outp "        \"~A\"\n" (cadr (assoc :out stanza)))
    (format outp "    ],\n")
    (format outp "    cmd  = \"~A\"\n" (cadr (assoc :cmd stanza)))
    (format outp ")\n")

    (if (list? stanza)
        (begin
          (format outp "## (\n")
          (for-each (lambda (sexp)
                      (format outp "##   ~A\n" sexp))
                    stanza)
          (format outp "## )\n")))
)

(define (starlark-emit-write-file-target outp stanza)
  ;; (format #t "starlark-emit-write-file-target: ~A\n" stanza)
  ;; (let ((libname (cdadr (assoc :name stanza)))
  ;;       (opts (stanza-opts stanza))
  ;;       (deps '("test-dep1" "test-dep2"))
  ;;       (modules '("test-mod1" "test-mod2"))
  ;;       (submodules (stanza-submodules typ stanza)))

  (format outp "###########\n")
  (format outp "write_file(\n")
  (format outp "    name     = \"~A\",\n"
          (string-append "write_"
                         (symbol->string (cadr (assoc :out stanza)))))
  (format outp "    out      = \"~A\",\n"
          (cadr (assoc :out stanza)))
  (format outp "    content  = \"\"\"\n")
  (format outp "~A" (cadr (assoc :str stanza)))
  (if (not (string-suffix? "\n" (cadr (assoc :str stanza))))
      (newline outp))
  (format outp "\"\"\"")
  (format outp ")")
  (newline outp)
  (newline outp)

  ;; debugging
  ;; (if (list? stanza)
  ;;     (begin
  ;;       (format outp "## (\n")
  ;;       (for-each (lambda (sexp)
  ;;                   (format outp "##   ~A\n" sexp))
  ;;                 stanza)
  ;;       (format outp "## )\n")))
  )

(define (starlark-emit-rule-targets outp stanzas)
  ;; (format #t "starlark-emit-rule-targets")
  (format outp "########################\n")
  (format outp "####  Rule Targets  ####")
  (newline outp)
  (newline outp)

  ;; same code as starlark-emit-aggregate-targets, but we want to put
  ;; aggregats and rules in different locations.
  (for-each (lambda (stanza)
              (case (car stanza)
                ((rule)
                 (starlark-emit-rule-target outp (cdr stanza)))
                ((:genrule)
                 (starlark-emit-genrule-target outp (cdr stanza)))
                ((:with-stdout-to)
                 (starlark-emit-with-stdout-to-target outp (cdr stanza)))
                ((:write-file)
                 (starlark-emit-write-file-target outp (cdr stanza)))
                (else
                 ;; skip
                 )))
            stanzas))

;; install targets - ignore
  ;; (if (assoc-in '(:stanzas install) (cdr dune-pkg-tbl))
  ;;     (begin
  ;;       (format outp "## install targets\n")
  ;;       (newline outp)))

(define (starlark-emit-build-files dune-pkg-tbls)
  (format #t "starlark-emit-build-files\n")
  (for-each
   (lambda (dune-pkg-tbl)
     (for-each
      (lambda (pkg-kv)
        ;; (format #t "pkg: ~A\n" (cdr pkg-kv))
        (let* ((fs-path (car pkg-kv))
               (stanzas-assoc (assoc :stanzas (cdr pkg-kv))))
          (format #t "emitting stanzas for: ~A\n" fs-path)
          ;; (display (format #f "stanzas: ~A" stanzas-assoc)) (newline)
          (if stanzas-assoc ;; assoc returns #f if not found
              (let* ((stanzas (cadr stanzas-assoc))
                     (build-file (string-append fs-path "/BUILD.obazl"))
                     ;; fs-path should already exist
                     (outp (open-output-file build-file)))
                ;; (display (format #f "Emitting ~A\n" build-file))

                (starlark-emit-build-file-hdr outp pkg-kv)
                ;; (newline outp)

                ;; (format #t "emitting executables\n")
                (starlark-emit-executable-targets outp fs-path stanzas)

                ;; (format #t "emitting aggregates\n")
                (starlark-emit-aggregate-targets outp fs-path stanzas)

                ;; (format #t "emitting module files\n")
                (starlark-emit-file-targets outp fs-path stanzas (cdr pkg-kv))
                (newline outp)

                ;; (format #t "emitting rules\n")
                (starlark-emit-rule-targets outp stanzas) ;; (cdr pkg-kv))
                (newline outp)

                (close-output-port outp)

                ;; (let ((stanzas (cdr (assoc :stanzas (cdr path_pkg))))
                ;;       (srcfiles (if-let ((srcs (assoc :srcfiles (cdr path_pkg))))
                ;;                         (cadr srcs)
                ;;                         '())))
                ;;   )

                ;; (let ((lib-stanzas (filter-stanzas :library stanzas)))
                ;;   (if (not (null? lib-stanzas))
                ;;       (emit-library-args fs-path lib-stanzas srcfiles out-port)))

                ;; (let ((exec-stanzas (filter-stanzas 'executable stanzas)))
                ;;   (if (not (null? exec-stanzas))
                ;;       (begin
                ;;         (emit-executable-args fs-path exec-stanzas srcfiles out-port))))

                ;; (let ((execs-stanzas (filter-stanzas 'executables stanzas)))
                ;;   (if (not (null? execs-stanzas))
                ;;       (emit-executables-args fs-path execs-stanzas srcfiles out-port)
                ;;         ))

                ))))
      (cadr dune-pkg-tbl)))
   dune-pkg-tbls)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (starlark-elaborate-pkg-tbls dune-pkg-tbls modules-tbl)
;;   (for-each
;;    (lambda (dune-pkg-tbl)
;;      (for-each
;;       ;; for each pkg:
;;       ;;    for each srcfile:
;;       ;;       get module name for srcfile
;;       ;;       get depslist from dependencies sexp
;;       ;;       for each dep in depslist:
;;       ;;          is dep in an -opened ns?
;;       ;;             for each ns listed in 'flags' fld of aggregator:
;;       ;;                lookup the ns in modules tbl, search its submods
;;       ;;             alternatively: keep list of nss with module in m-tbl
;;       ;;             if :ns use it else use :name
;;       ;;             get path for label
;;       ;;       pull opts from aggregator module in dune-pkg
;;       ;;    for each library/executable stanza:
;;       ;;       for each srcdep ('modules' fld):
;;       ;;       for each libdep ('libraries' fld):

;;       (lambda (pkg-kv)
;;         ;; (format #t "pkg: ~A\n" (cdr pkg-kv))
;;         (let* ((fs-path (car pkg-kv))
;;                (pkg (cdr pkg-kv))
;;                (stanzas-assoc (assoc :stanzas pkg)))
;;           ;; (format #f "fs-path: ~A" fs-path)
;;           ;; (display (format #f "stanzas: ~A" stanzas-assoc)) (newline)
;;           (if stanzas-assoc ;; assoc returns #f if not found
;;               (let* ((stanzas (cadr stanzas-assoc)))

;;                 (starlark-emit-build-file-hdr outp pkg-kv)
;;                 ;; (newline outp)

;;                 (starlark-emit-aggregate-targets outp stanzas) ;; pkg-kv)
;;                 (newline outp)

;;                 ;; (cdr pkg-kv) == pkg

;;                 (starlark-emit-file-targets outp stanzas (cdr pkg-kv))
;;                 (newline outp)

;;                 (starlark-emit-rule-targets outp stanzas) ;; (cdr pkg-kv))
;;                 (newline outp)

;;                 (close-output-port outp)

;;                 ;; (let ((stanzas (cdr (assoc :stanzas (cdr path_pkg))))
;;                 ;;       (srcfiles (if-let ((srcs (assoc :srcfiles (cdr path_pkg))))
;;                 ;;                         (cadr srcs)
;;                 ;;                         '())))
;;                 ;;   )

;;                 ;; (let ((lib-stanzas (filter-stanzas :library stanzas)))
;;                 ;;   (if (not (null? lib-stanzas))
;;                 ;;       (emit-library-args fs-path lib-stanzas srcfiles out-port)))

;;                 ;; (let ((exec-stanzas (filter-stanzas 'executable stanzas)))
;;                 ;;   (if (not (null? exec-stanzas))
;;                 ;;       (begin
;;                 ;;         (emit-executable-args fs-path exec-stanzas srcfiles out-port))))

;;                 ;; (let ((execs-stanzas (filter-stanzas 'executables stanzas)))
;;                 ;;   (if (not (null? execs-stanzas))
;;                 ;;       (emit-executables-args fs-path execs-stanzas srcfiles out-port)
;;                 ;;         ))

;;                 ))))
;;       dune-pkg-tbl))
;;    dune-pkg-tbls)
;;   )
