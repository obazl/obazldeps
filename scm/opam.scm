(load "string.scm")

(define (opam-exports->tezos-exports)
  (let* ((fname ".opam.d/opam-exports.scm")
         (opam-exports-port (open-input-file fname))
         (tezos-exports (make-hash-table))
         (export-syms (catch 'read-error
                             (lambda () (read opam-exports-port))
                             (lambda args
                               (handle-read-error path fname args))))
         (export-syms (sort! export-syms sym<?))
         (exports (make-hash-table)))
    (for-each (lambda (export)
                ;; WARNING: sys call to opam is extremely slow
                ;; optimize: read and parse {SWITCH}/.opam-switch/switch-state
                (let* ((version (system
                                 (format #f "opam var ~S:version" export)
                                 #t))
                       (version (string-trim '(#\space #\newline) version))
                       (vsym version)) ;;(string->symbol version)))
                  (format #t "~A: ~A\n" export vsym)
                  ;;(cons export vsym)
                  ;;(string->symbol
                  (hash-table-set! exports
                                   export
                                   (format #f "~A.~A" export version))))
              export-syms)
    (format #t "EXPORTS: ~A\n" exports)
    (with-output-to-file ".opam.d/tezos.opam.export"
      (lambda ()
        (display "opam-version: \"2.0\"\n")
        (display "compiler: [\n")
        ;; FIXME: do not hardcode these?
        (display " \"base-bigarray.base\"\n")
        (display " \"base-threads.base\"\n")
        (display " \"base-unix.base\"\n")
        (format #t " \"~A\"\n" (hash-table-ref exports 'ocaml))
        (format #t " \"~A\"\n" (hash-table-ref exports 'ocaml-base-compiler))
        (format #t " \"~A\"\n" (hash-table-ref exports 'ocaml-config))
        (display "]\n")
        (display "roots: [\n")
        (for-each (lambda (export-sym)
                    ;; e.g.   "alcotest.1.1.0"
                    (format #t " ~S"
                            (hash-table-ref exports export-sym))
                    (newline))
                  export-syms)
        (display "]\n")
        (display "installed: [\n")
        (for-each (lambda (export-sym)
                    (format #t " ~S"
                            (hash-table-ref exports export-sym))
                    (newline))
                    ;; (format #t " ~S" (cdr export)) (newline))
                  export-syms)
        (display "]\n")
        ))))
    ;;       (for-each (lambda (export)
    ;;                   (write export) (newline))
    ;;                 (sort! (hash-table-keys tezos-exports) sym<?))))
    ;;   (for-each (lambda (entry)
    ;;               (hash-table-set! resolver
    ;;                                (car entry) (cdr entry)))
    ;;             entries)
    ;;   )
    ;; resolver))


(define ocamlfind-pkg-syms
  ;; pkg ocamlfind installs a bunch o META files:
  ;;   https://github.com/ocaml/ocamlfind/tree/master/site-lib-src
  `(bigarray bytes camlp4.309 camlp4.310 compiler-libs dbm dynlink
             findlib graphics labltk num-top num ocamlbuild ocamldoc
             raw_spacetime stdlib str threads unix))

(define opam-remappings
  `((bigarray . ocamlfind)
    (bytes . ocamlfind)
    (camlp4.309 . ocamlfind)
    (camlp4.310 . ocamlfind)
    (compiler-libs . ocamlfind)
    (dbm . ocamlfind)
    (dynlink . ocamlfind)
    (findlib . ocamlfind)
    (graphics . ocamlfind)
    (labltk . ocamlfind)
    (num-top . ocamlfind)
    (num . ocamlfind)
    (ocamlbuild . ocamlfind)
    (ocamldoc . ocamlfind)
    (raw_spacetime . ocamlfind)
    (stdlib . ocamlfind)
    (str . ocamlfind)
    (threads . ocamlfind)
    (unix . ocamlfind)

    (oUnit . ounit2)
    (ocplib-ocamlres . ocp-ocamlres)
    (zip . camlzip)))

(define (make-opam-resolver)
  ;; convert opam_resolver_raw.scm to hash table to eliminate dups
  (let* ((fname ".opam.d/opam_resolver_raw.scm")
         (opam-resolver-port (open-input-file fname))
         (exports-tbl (make-hash-table))
         (resolver (make-hash-table)))
    (let-values (((entries exports)
                  (let recur ((sexp-list '())
                              (exports '()))
                    (let ((next-sexp
                           (catch 'read-error
                                  (lambda () (read opam-resolver-port))
                                  (lambda args
                                    (handle-read-error path fname args)))))
                      (if (eof-object? next-sexp)
                          (begin
                            (close-input-port opam-resolver-port)
                            (values sexp-list exports))
                          (begin
                            (recur
                             (cons next-sexp sexp-list)
                             (cons (car next-sexp) exports))))))))
      (for-each (lambda (export)
                  (let* ((s (symbol->string export))
                         (i (string-index s
                                          (lambda (ch)
                                            (equal? ch #\.))))
                         (exp (string->symbol
                               (if i (string-take s i) s))))
                    ;; (if (member exp ocamlfind-pkg-syms)
                    (if-let ((pkg (assoc exp opam-remappings)))
                        (hash-table-set! exports-tbl (cdr pkg) #t)
                        (hash-table-set! exports-tbl exp #t))))
                exports)
      ;; for the 'compiler' section of opam.export
      (hash-table-set! exports-tbl 'ocaml #t)
      (hash-table-set! exports-tbl 'ocaml-base-compiler #t)
      (hash-table-set! exports-tbl 'ocaml-config #t)

      (with-output-to-file ".opam.d/opam-exports.scm"
        (lambda ()
          (display "(")
          (for-each (lambda (export)
                      (write export) (newline))
                    (sort! (hash-table-keys exports-tbl) sym<?))
          (display ")")))
      (for-each (lambda (entry)
                  (hash-table-set! resolver
                                   (car entry) (cdr entry)))
                entries)
      )
    resolver))

(define (module->opam-label module)
  (string-append "@" module "//:" module))

;; (define switch-prefix (string-right-trim " \n" (system "opam var lib" #t)))

;; returns string
;; (define (maybe-opam module)
;;   ;; (format #t "maybe-opam: [~A] :: ~A\n" switch-prefix module)
;;   (let* ((module-str (if (symbol? module)
;;                                   (symbol->string module)
;;                                   module)))
;;     ;; (enslash module-str) ;; . => /

;;   (string-set! module-str 0
;;                (char-downcase (string-ref module-str 0)))

;;   ;; (if (string-index (symbol->string module) (lambda (ch) (equal? ch #\.)))
;;   ;;     (format #t "ENSLASH: ~A => ~A\n" module module-str))

;;   ;; OPAM_SWITCH_PREFIX must be correctly set for ocamlfind:
;;   (let* ((pkg-path (string-append switch-prefix "/" module-str))

;;          ;; 'ocamlfind query' is too slow!
;;          ;; unfortunately, pkg names do not map cleanly to fs paths,
;;          ;; e.g. mtime.clock.os => mtime/os
;;          ;; so we're stuck with ocamlfind
;;          (ocamlfind-pkg (system
;;                          (string-append
;;                           "OPAM_SWITCH_PREFIX=" switch-prefix
;;                           " ocamlfind query " module-str
;;                           " 2> /dev/null")
;;                          ;; #t: capture output string and return it
;;                          )))
;;     ;; (format #t "ocamlfind-pkg: ~A\n" ocamlfind-pkg)
;;     ;; (if (equal? ocamlfind-pkg 0)

;;     ;; (if (file-exists? pkg-path)
;;     ;; (if (char=? (string-ref ocamlfind-pkg 0) #\/)
;;       (if (equal? ocamlfind-pkg 0)
;;           module-str
;;           (begin
;;             ;; (if (string-index (symbol->string module)
;;             ;;                   (lambda (ch) (equal? ch #\.)))
;;             ;;     (format #t "OPAM fail: ~A\n" pkg-path))
;;             #f)))))

          ;; (let ((module-str (endash module-str)))
          ;;   (if (file-exists? (string-append "/Users/gar/.opam/4.10/lib/"
          ;;                                    module-str))
          ;;       module-str
          ;;       #f))))))

;; on success returns obazl label, else returns false
;; (define (update-opam-table module) ;; opam-tbl module)
;;   ;; (format #t "update-opam-table: ~A\n" module)
;;   (let* ((module module)
;;          (resolved (opam-tbl module)))
;;     (if resolved
;;         module
;;         (if-let ((opam (maybe-opam module)))
;;                 (let ((lbl (module->opam-label opam)))
;;                   ;; (format #t "OPAM: ~A: ~A\n" module opam)
;;                   (hash-table-set! opam-tbl (string->symbol opam) lbl)
;;                   (hash-table-set! opam-tbl module lbl)
;;                   lbl)
;;                 #f))))

;; returns #f if unresolvable as opam pkg
(define (resolve-opam module)
  (let ((resolved
         (if-let ((opam (opam-resolver module)))
                 (symbol->string opam)
                 (if-let ((opam (opam-resolver
                                 (normalize-module-name module))))
                         (symbol->string opam)
                         #f))))
    ;; (format #t "OPAM resolved: ~A: ~A\n" module resolved)
    resolved))

;; (define (resolve-opam module)
;;   ;; (format #t "resolve-opam: ~A\n" module)
;;   (let ((resolved
;;          (if-let ((opam (opam-tbl module)))
;;                  opam
;;                  (if-let ((opam (opam-tbl (normalize-module-name module))))
;;                          opam
;;                          (update-opam-table module)))))
;;     ;; (format #t "OPAM resolved: ~A: ~A\n" module resolved)
;;     resolved))

;; (define (codept->opam-table codept-sexp)
;;   (let* ((unresolved-modules (codept->unknown-modules codept-sexp))
;;          (opam-tbl (make-hash-table 1024 equal?)))
;;     (for-each (lambda (unresolved-module)
;;                 ;; (format #t "unresolved-module: ~A\n" unresolved-module)
;;                 ;; (format #t "  module: ~A\n" (caadar unresolved-module))
;;                 ;; unresolved-module :: ((module (<name>)) (ml ...) (mli ...))

;;                 (update-opam-table opam-tbl (car unresolved-module)))
;;          unresolved-modules)
;;     opam-tbl))

