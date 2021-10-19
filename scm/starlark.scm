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

(define (stanza-deps fs-path modname stanza)
  ;; (if (equal? modname 'tezos-protocol-environment-sigs)
  ;;     (format #t "~A stanza-deps: ~A\n" modname stanza))
  ;; NB: resolved deps use (always?) public_name
  ;; (format #t "STZ: ~A\n" stanza)
  (if-let ((deps (assoc-in '(:deps :libs) stanza)))
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
                   (if-let ((modname (names-tbl dep)))
                                      ;; (private-name->public-name dep))))
                           (begin
                             ;;(format #t "PUBNAME: ~A\n" pubname)
                             ;; (format #t "MODNAME: ~A\n" modname)
                             (if-let ((modspec (modules-tbl modname)))
                                     (string-append
                                      "//" (cadr (assoc :path modspec))
                                      ":" (symbol->string dep))
                                     ;; ":" (symbol->string pname))
                                     (string-append "NO MODSPEC: "
                                                    (symbol->string dep))))

                   (if-let ((modname (names-tbl ;; public-name->module-name
                                      (private-name->public-name dep))))
                                      ;; (private-name->public-name dep))))
                           (begin
                             ;;(format #t "PUBNAME: ~A\n" pubname)
                             ;; (format #t "PMODNAME: ~A\n" modname)
                             (if-let ((modspec (modules-tbl modname)))
                                     (string-append
                                      (if (string=?
                                           fs-path
                                           (cadr (assoc :path modspec)))
                                          ":"
                                          (string-append
                                           "//" (cadr (assoc :path modspec))))
                                      (symbol->string
                                       (private-name->public-name dep))
                                      )
                                     ;; ":" (symbol->string pname))
                                     (string-append "NO MODSPEC: "
                                                    (symbol->string dep))))

                           (if-let ((opam (resolve-opam dep)))
                                   opam
                                   ;; no opam
                                   (if-let ((stdlib (resolve-stdlib dep)))
                                           stdlib
                                           (format #t "UNRESOLVED: ~A: ~A\n"
                                                   modname dep))))))
                 (cadr deps)))
          '()))

(define (stanza-opts stanza)
  (if-let (;;(opts (assoc :opts stanza))
           (flags (assoc-in '(:opts :flags) stanza)))
          (begin
            ;; (format #t "FLAGS: ~A\n" (cdr flags))
            (if (member :standard flags)
                (concatenate (standard-flags)
                             (remove :item :standard (cdr flags)))
                (cdr flags)))
          '()))

(define (expand-modules-list modules)
  ;; (format #t "expand-modules-list: ~A\n" modules)
  (if-let ((sublist (member :standard modules)))
      (begin
        (format #t "SUBLIST ~A\n" sublist)
        sublist)
      (begin
        (format #t "HUH?\n")
        modules)))

;; returns list
(define (stanza-modules libname stanza)
  ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
  ;; (if (equal? libname 'Tezos_base)
  ;;       (format #t "stanza-submodules ~A: ~A\n" libname stanza))
  (let ((modules-direct (if-let (directs (assoc-in
                                          '(:modules :direct) stanza))
                                (cdr directs) '()))
        (modules-indirect (if-let (indirects (assoc-in
                                       '(:modules :indirect) stanza))
                                  (cdr indirects) '())))
    ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
    ;; (if (equal? libname 'Tezos_base)
        ;; (begin
        ;;   (format #t "BASE direct subms: ~A\n" modules-direct)
        ;;   (format #t "BASE indirect subms: ~A\n" modules-indirect))
    (concatenate modules-direct modules-indirect)))

(define (stanza-submodules libname stanza)
  ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
  ;; (if (equal? libname 'Tezos_base)
  ;;       (format #t "stanza-submodules ~A: ~A\n" libname stanza))
  (let ((modules-direct (if-let (directs (assoc-in
                                          '(:submodules :direct) stanza))
                                (cdr directs) '()))
        (modules-indirect (if-let (indirects (assoc-in
                                       '(:submodules :indirect) stanza))
                                  (cdr indirects) '())))
    ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
    ;; (if (equal? libname 'Tezos_base)
        ;; (begin
        ;;   (format #t "BASE direct subms: ~A\n" modules-direct)
        ;;   (format #t "BASE indirect subms: ~A\n" modules-indirect))
    (concatenate modules-direct modules-indirect)))

(define (explicit-ns? modname pubname)
  (if (equal? modname pubname)
      #f
      (let ((s1 (if (symbol? modname) (symbol->string modname) modname))
            (s2 (if (symbol? pubname) (symbol->string pubname) pubname)))
        (not (and (string=? (substring s1 1) (substring s2 1))
                  (char=? (char-upcase (string-ref s1 0))
                          (char-upcase (string-ref s2 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (starlark-emit-executables outp fs-path stanza)
  (format #t "starlark-emit-executables ~A\n" stanza)

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

(define (starlark-emit-executable-target outp fs-path stanza)
  (format #t "starlark-emit-executable-target ~A\n" stanza)
  (let* (;; (libname (cdadr (assoc :name stanza)))
         (modname (if-let ((modname (assoc-in '(:name :module) stanza)))
                          (cadr modname) #f))
         (privname (cadr (assoc-in '(:name :private) stanza)))
         ;; (pubname (if-let ((pn (assoc-in '(:name :public) stanza)))
         ;;                  (cadr pn)
         ;;                  privname))
         (pubname (if-let ((pn (assoc :public_name stanza)))
                          (cadr pn)
                          privname))
         (opts (stanza-opts stanza))
         (deps (stanza-deps fs-path pubname stanza))
         (submodules (stanza-submodules modname stanza))
         (modules (stanza-modules modname stanza))
         )

    (format #t "MODULES: ~A\n" modules)
    (format #t "SUBMs: ~A\n" submodules)
    ;; (format outp "~A = [\n" (name->opts-sym modname))
    ;; (for-each (lambda (opt)
    ;;             (format outp "    \"~A\",\n" opt)
    ;;             )
    ;;           opts)
    ;; (format outp "]\n")
    ;; (newline outp)

    ;; (format outp "~A = [\n" (name->deps-sym modname))
    ;; (for-each (lambda (dep)
    ;;             (format outp "    \"~A\",\n" dep)
    ;;           )
    ;;           deps)
    ;; (format outp "]\n")
    ;; (newline outp)

    (begin
      (format outp "#################\n")
      (format outp "ocaml_executable(\n")
      (format outp "    name    = \"~A\",\n" pubname)
      (format outp "    modules = [\n")
      (for-each (lambda (mod)
                  (format outp "        \":~A\",\n"
                          (symbol->string
                           (normalize-module-name mod))
                          ))
                modules)
      ;; (for-each (lambda (mod)
      ;;             (format outp "        \"~A\",\n" mod))
      ;;           modules)
      (format outp "    ],\n")
      (format outp ")\n\n")
      ;;(format outp "#############################\n")
      ))
  )

(define (starlark-emit-executable-targets outp fs-path stanzas)
  (format #t "starlark-emit-executable-targets ~A\n" ;; : ~A\n"
          fs-path)
  ;; (format #t "stanzas: ~A\n" stanzas)
  (format outp "##############################\n")
  (format outp "####  Executable Targets  ####\n")
  (for-each (lambda (stanza)
              (format #t "stanza x: ~A ~A\n" fs-path (car stanza))
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
  (format #t "starlark-emit-aggregate-target ~A\n" fs-path)
  (let* (;; (libname (cdadr (assoc :name stanza)))
        (modname (cadr (assoc-in '(:name :module) stanza)))
        (pubname (cadr (assoc-in '(:public_name) stanza)))
        (explicit-ns (explicit-ns? modname pubname))
        ;; (_ (format #t "emit-agg pubname: ~A\n" pubname))
        (opts (stanza-opts stanza))
        (deps (stanza-deps fs-path pubname stanza))
        (submodules (stanza-submodules modname stanza)))

    ;; (format #t "SUBMs: ~A\n" submodules)
    (format outp "~A = [\n" (name->opts-sym modname))
    (for-each (lambda (opt)
                (format outp "    \"~A\",\n" opt)
                )
              opts)
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
    )
  )

(define (starlark-emit-stanza-deps-and-flags outp typ stanza)
  (format #t "starlark-emit-stanza-deps-and-flags\n")
  (let ((modname (cadr (assoc-in '(:name :module) stanza)))
        (opts (stanza-opts stanza))
        (deps (stanza-deps modname stanza))
        (submodules (stanza-submodules modname typ stanza)))

    (format outp "~A = [\n" (name->opts-sym modname))
    (for-each (lambda (opt)
                (format outp "    \"~A\",\n" opt)
                )
              opts)
    (format outp "]\n")
    (newline outp)

    (format outp "~A = [\n" (name->deps-sym modname))
    (for-each (lambda (dep)
                (format outp "    \"~A\",\n" dep))
              deps)
    (format outp "]\n")
    (newline outp)
    ))

(define (library-wrapped? stanza)
  (if-let ((wrapped (assoc 'wrapped (cadr stanza))))
          (if (equal? 'false (cadr wrapped))
              #f
              #t)
          #t))

(define (starlark-emit-aggregate-targets outp fs-path stanzas)
  (format #t "starlark-emit-aggregate-targets ~A\n" fs-path)
  (format outp "#############################\n")
  (format outp "####  Aggregate Targets  ####\n")
  (for-each (lambda (stanza)
              (case (car stanza)
                ((:library)
                 ;; FIXME: if no sources, then it's a placeholder for
                 ;; install, so skip? e.g. tezos/src/tooling

                 (starlark-emit-aggregate-target
                  outp
                  (if (library-wrapped? stanza) 'ns-archive :library)
                  fs-path
                  (cadr stanza))

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
  (case (car stanza)
    ((:library) #t)
    (else #f)))

(define (module-name->aggregate-module-name _stanzas module)
  ;; (newline)
  ;; (format #t "module-name->aggregate-module-name: ~A\n" module)
  ;; (if (equal? module 'Shell_context)
  ;;     (format #t "stanzas: ~A\n" _stanzas))
  (let ((result
         (let recur ((stanzas _stanzas))
           ;; (if (not (null? stanzas))
           ;;     (format #t " car stanzas: ~A\n" (car stanzas)))
           (if (null? stanzas)
               #f
               (if (aggregate-stanza? (car stanzas))
                   (let* ((stanza (cadr (car stanzas)))
                          (modules (if-let ((ms (assoc-in
                                                 '(:submodules :direct) stanza)))
                                           (cdr ms)
                                           #f)))
                     ;; (if (equal? module 'Shell_context)
                     ;; (format #t "MODULES: ~A\n" modules) ;;)
                     (if modules
                         (begin
                           (if (member module modules)
                               (let ((nm (assoc-in '(:name :module) stanza)))
                                 (begin
                                   ;; (display (format #f "HIT: ~A : ~A :: ~A\n"
                                   ;;                  module nm (cdadr nm)))
                                   (cadr nm)))
                               (recur (cdr stanzas))))
                         (if (equal? :library (car stanza))
                             (let ((nm (assoc-in '(:name :module) stanza)))
                               (cadr nm))
                             ;; recur??
                             #f)))
                   #f)))))
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
               (let* ((stanza (cadr (car stanzas)))
                      (modules (if-let ((ms (assoc-in
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
                         #f)))))))
    result))

(define (resolve-libdep ns module-name dep stanzas)
  ;; module-name in ns depends on dep; search ns 'libraries' fld
  ;; (format #t "resolve-libdep ~A[~A]: ~A\n" ns module-name (car dep))
          ;; module-name dep ns)
  ;; NB: libraries fld uses public_names
  ;; example: Tezos_proxy[Light] depends on RPC_context
  ;; (modules-tbl 'Tezos_proxy) => :deps list contains tezos-rpc
  ;; (public-names-lookup 'tezos-rpc) => Tezos_rpc
  ;; (modules-tbl 'Tezos_rpc) => :submodules assoc list contains RPC_context
  ;; result: Light depends on Tezos_rpc
  (let* ((modspec (modules-tbl ns)) ;; e.g. Tezos_proxy
         (libdeps (assoc :deps modspec))) ;; :deps of Tezos_proxy
    ;; (format #t "modspec: ~A\n" modspec)
    ;; (format #t "libdeps: ~A\n" libdeps)
    ;; (if libdeps
    ;;     (format #t "  NS LIBDEPS: ~A:: ~A\n" ns (cadr libdeps)))
    (if libdeps
        (let recur ((libdeps (cadr libdeps))) ;; libdeps=list of public_names
          (if (null? libdeps)
              #f
              (begin
                ;; (format #t "LIBDEP: ~A\n" (car libdeps))
                (let ((libdep-module (private-name->public-name
                                      (car libdeps))))
                  ;; e.g. tezos-rpc => Tezos_rpc
                  (if-let ((libdep-mtbl (modules-tbl libdep-module)))
                          (if-let ((libdep-submods
                                    (assoc :submodules libdep-mtbl)))
                                  (begin
                                    ;; (format #t "lipdep submods: ~A:: ~A\n"
                                    ;;         libdep-module libdep-submods)
                                    (if (member (car dep) libdep-submods)
                                        (begin
                                          ;; (format #t "HIT: ~A[~A] :: ~A\n"
                                          ;;         ns (car dep) libdep-module)
                                          libdep-module)
                                        ;; dep not in submods of this libdep
                                        (recur (cdr libdeps))))
                                  ;; :submodules no found in libdep mtbl
                                  (recur (cdr libdeps)))
                          ;; FIXME not in modules-tbl - error?
                          (recur (cdr libdeps)))))))
        ;; ns not in modules-tbl - error?
        #f)))

(define (resolve-dep ns module-name dep stanzas)
  ;; (format #t "resolve-dep opens: ~A:: ~A\n" ns opens) ;; dep)
  ;; (format #t "modules-tbl: ~A\n" modules-tbl)

  ;; FIXME: handle compound deps, e,g. (Tezos_client_base Client_context)

  (let ((opens (module-name->opens-list stanzas module-name)))
    (if (equal? ns (car dep))
        (string-append ":" (symbol->string (cadr dep)))
        ;; lookup dep in -open modules of aggregator
        (if opens
            (let recur ((opens (cadr opens)))
              ;; (format #t "recur ~A\n" opens)
              (if (null? opens)
                  ;; not resolved, try 'libraries'
                  (if-let ((libdep
                            (resolve-libdep ns module-name dep stanzas)))
                          libdep
                          (if-let ((opam (resolve-opam dep)))
                                  opam
                                  '()))
                  ;; else try next opened dep
                  (if-let ((module-alist (modules-tbl (car opens))))
                          (if-let ((submods (assoc :submodules module-alist)))
                                  ;; (list opens (car dep) submods)
                                  (if-let ((hit (member
                                                 ;; FIXME: namespaced deps
                                                 (car dep)
                                                 (cdr submods))))
                                          (begin
                                            ;; (format #t "HIT ~A:: ~A: ~A\n"
                                            ;;         (car opens) (car dep) hit)
                                            (car opens))
                                          (recur (cdr opens)))
                                  (recur (cdr opens)))
                          ;; not in this opened module, try the next
                          (recur (cdr opens)))))
            ;; no -open flags - try libdeps, then opam
            ))))

(define (starlark-emit-file-targets outp fs-path stanzas dune-pkg)
  (format #t "starlark-emit-file-targets: ~A\n" fs-path) ;; dune-pkg)
  (format outp "#############################\n")
  (format outp "####  Singleton Targets  ####\n")
  (newline outp)
  (let ((srcs (if-let ((srcs (assoc-in '(:srcfiles :ocaml) dune-pkg)))
                      (sort! (cadr srcs) string<?)
                      '())))
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
                  (let* ((aggregate-module-name
                          (module-name->aggregate-module-name stanzas mname)))
                    ;; (format #t "aggregate-module-name: ~A\n"
                    ;;         aggregate-module-name)
                    (if (string-suffix? ".ml" srcfile)
                        (begin
                          (format outp "ocaml_module(\n")
                          (format outp "    name   = \"~A\",\n" mname)
                          (format outp "    struct = \"~A\",\n" srcfile)
                          (if (member (string-append
                                       (symbol->string mname) "i")
                                      srcs)
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
                    (format outp "    opts   = ~A,\n"
                            (if aggregate-module-name
                                (name->opts-sym aggregate-module-name)
                                "[]"))
                    ;; (stanzas-module->opts-sym stanzas mname))
                    ;; (if-let ((sym
                    ;;           (stanzas-module->opts-sym
                    ;;            stanzas mname)))
                    ;;         sym
                    ;;         sym))

                    (format outp "    deps   = ~A,\n"
                            (if aggregate-module-name
                                (name->deps-sym aggregate-module-name)
                                ;; (string-append "DEPS_"
                                ;;            (symbol->string
                                ;;             aggregate-module-name))
                                "[]"))

                  ;; (format outp "    deps   = [\n")
                  ;; (let* ((deps (codept-srcfile->depslist
                  ;;              (string-append fs-path "/" srcfile)
                  ;;              codept-sexp))
                  ;;        (deps (if (null? deps) '() deps)))
                  ;;   (for-each (lambda (dep)
                  ;;               (format outp "       \"~A\",        # ~A\n"
                  ;;                       (resolve-dep aggregate-module-name
                  ;;                                     mname dep stanzas)
                  ;;                       dep)
                  ;;               )
                  ;;           deps))
                  ;; (format outp "    ]\n")

                  (format outp ")\n\n")
                  )))
              srcs)
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
  (format #t "starlark-emit-with-stdout-to-target: ~A\n" stanza)
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
  (for-each
   (lambda (dune-pkg-tbl)
     (for-each
      (lambda (pkg-kv)
        ;; (format #t "pkg: ~A\n" (cdr pkg-kv))
        (let* ((fs-path (car pkg-kv))
               (stanzas-assoc (assoc :stanzas (cdr pkg-kv))))
          (format #t "fs-path: ~A" fs-path)
          ;; (display (format #f "stanzas: ~A" stanzas-assoc)) (newline)
          (if stanzas-assoc ;; assoc returns #f if not found
              (let* ((stanzas (cadr stanzas-assoc))
                     (build-file (string-append fs-path "/BUILD.obazl"))
                     ;; fs-path should already exist
                     (outp (open-output-file build-file)))
                ;; (display (format #f "Emitting ~A\n" build-file))

                (starlark-emit-build-file-hdr outp pkg-kv)
                ;; (newline outp)

                (starlark-emit-executable-targets outp fs-path stanzas)
                (newline outp)

                (starlark-emit-aggregate-targets outp fs-path stanzas)
                (newline outp)

                ;; (cdr pkg-kv) == pkg

                (starlark-emit-file-targets outp fs-path stanzas (cdr pkg-kv))
                (newline outp)

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
