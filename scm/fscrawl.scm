(load "codept.scm")
(load "string.scm")
(load "utils.scm")

;; NB: includes sig-only files
(define (files->modules files)
  (let* ((mfiles (filter (lambda (f)
                           (or
                            ;; (string-suffix? ".mli" f)
                            (string-suffix? ".ml" f)))
                           files))
         (modules (map file-name->module-name mfiles)))
    ;; (format #t "files->modules: ~A\n" modules)
    modules))

(define (files->sigs files)
  (let* ((sigfiles (filter (lambda (f) (string-suffix? ".mli" f)) files))
         (sigs (map file-name->module-name sigfiles)))
    ;; (format #t "sigs: ~A\n" sigs)
    sigs))

;; (define (emit-codept-args outp path files)
;;   ;; (format #t "cd path ~A (~A)\n" path (type-of path))
;;   (if files
;;       (let ((group (if (equal? "." path) "_ROOT" path)))
;;         ;; (format #t "cdept group: '~A'\n" group)
;;         (if (not (string-prefix? "testsuite" group))
;;             (begin
;;               (format outp "~A[" (string-tr group #\/ #\_))
;;               (for-each (lambda (f)
;;                           (if (or (string-suffix? ".mli" f) (string-suffix? ".ml" f))
;;                               (format outp "~A/~A," path f)))
;;                         files)
;;               (format outp "]\n"))))))

(define (emit-buildfile-header outp files modules sigs)
  (if (not (and (null? modules) (null? sigs)))
      (begin
        (format outp "## GENERATED FILE ##\n")
        (newline outp)

        (let ((sig? (not (null? sigs)))
              (struct? (any (lambda (f) (string-suffix? ".ml" f)) files)))
          (if (or sig? struct?)
              (begin
                (format outp "load(\"@obazl_rules_ocaml//ocaml:rules.bzl\",\n")
                (format outp "     \"ocaml_library\",\n")
                (if struct?
                    (format outp "     \"ocaml_module\",\n"))
                (if sig?
                    (format outp "     \"ocaml_signature\",\n"))
                (format outp ")\n")
                (newline outp)
                (if struct?
                    (format outp "MODULE_OPTS = [\"-nostdlib\"]\n"))
                (if sig?
                    (format outp "SIG_OPTS = [\"-nostdlib\"]\n"))
                (newline outp)
                )
              )))))

;; NB: library includes sig-only "modules"
(define (emit-library-target outp dir modules sigs)
  (if (not (and (null? modules) (null? sigs)))
      (let ((sig-widows (filter (lambda (sig)
                                  (not (member sig modules)))
                                sigs)))
        (format outp "ocaml_library(\n")
        (format outp "    name = \"~A_lib\",\n"
                (string-tr dir #\/ #\_))
        (if (not (null? modules))
            (begin
              (format outp "    modules  = [\n")
              (for-each (lambda (module)
                          (format outp "        \":~A\",\n" module))
                        modules)
              (format outp "    ],\n")))
        (if (not (null? sig-widows))
            (begin
              (format outp "    signatures  = [\n")
              (for-each (lambda (sig)
                          (format outp "        \":~A_cmi\",\n" sig))
                        sig-widows)
              (format outp "    ],\n")))
        (format outp ")\n")
        (newline outp))))

(define (emit-signature-target outp path f codept-alist)
  (let ((module (file-name->module-name f))
        (deps (if (not (string-prefix? "testsuite" path))
                  (file->deps path f)
                  '())))

    (format outp "ocaml_signature(\n")
    (format outp "    name = \"~A_cmi\",\n" module)
    (format outp "    src  = \"~A\",\n" f)
    (format outp "    opts = SIG_OPTS,\n")

    (format outp "    deps   = [\n")
    (for-each (lambda (dep)
                (if (> (length dep) 1)
                    (if (pair? (cdr dep))
                        (format outp
                                "        \":~A\", ## ~A\n" (cadr dep) dep)
                        ;; (format outp
                        ;;         "        \":~A\", ## ~A\n" (car dep) dep)))
                        (format outp "ERROR:        ## ~A\n" dep))
                    (format outp "        ## ~A\n" dep)))
              deps)
    (format outp "    ]\n")

    (format outp ")\n")
    (newline outp)))

(define (find-if f . args)
  (call-with-exit
   (lambda (return)
     (apply for-each (lambda main-args
		       (if (apply f main-args)
			   (apply return main-args)))
	    args))))

(define (file->deps-alist path f deps-alist-list)
  (if-let ((r (find-if (lambda (e)
                         (let* ((t (cadar e))
                                (t (if (symbol? t) (symbol->string t) t)))
                         (equal? t (string-append path "/" f))))
                       deps-alist-list)))
          (begin
            (format #t "deps-alist ~A: ~A\n" f r)
            r)
          '()))
  ;; (filter (lambda (deps-alist)
  ;;           (if-let ((x (assoc `file deps-alist)))
  ;;                   ,)
  ;;         deps-alist-list))

;; (define (module->deps path f)
;;   (if-let ((ctx (hash-table-ref modules-tbl module)))
;;           ctx
;;           (error 'missing-module
;;                  (format #f "missing module: ~A\n" module))))

(define (file->deps path f)
  (if-let ((ctx (hash-table-ref modules-tbl
                                (string->symbol
                                 (string-append path "/" f)))))
          (map (lambda (dep)
                 dep)
               ctx)
          (error 'missing-module
                 (format #f "missing entry: ~A/~A\n"
                         path f))))

          ;; (deps-alist-list (cadr deps))
          ;; (mdeps-alist (file->deps-alist path f deps-alist-list))
          ;; (mdeps-list (if mdeps-alist
          ;;                 (if-let ((mdeps-assoc (assoc 'deps mdeps-alist)))
          ;;                         (cadr mdeps-assoc)
          ;;                         '())
          ;;                 '()))

(define (emit-module-target outp path f sigs codept-alist)
  (let* ((module (file-name->module-name f))
         (deps (if (not (string-prefix? "testsuite" path))
                   (file->deps path f)
                   '())))
    (format #t "DEPS: ~A\n" deps)
    ;; (format #t "DEPS-alist-list: ~A\n" deps-alist-list)
    ;; (format #t "MDEPS: ~A\n" mdeps-list)

    (format outp "ocaml_module(\n")
    (format outp "    name   = \"~A\",\n" module)
    (format outp "    struct = \"~A\",\n" f)
    (if (member module sigs)
        (format outp "    sig    = \"~A_cmi\",\n" module))
    (format outp "    opts = MODULE_OPTS,\n")

    (format outp "    deps   = [\n")
    (for-each (lambda (dep)
                (if (> (length dep) 1)
                    (if (pair? (cdr dep))
                        (format outp
                                "        \":~A\", ## ~A\n" (cadr dep) dep)
                        ;; (format outp
                        ;;         "        \":~A\", ## ~A\n" (car dep) dep)))
                        (format outp "ERROR:        ## ~A\n" dep))
                    (format outp "        ## ~A\n" dep)))
              deps)
    (format outp "    ]\n")

    (format outp ")\n")
    (newline outp)))

;; (define (emit-file-target outp f)
;;   ;; (format #t "emit-file-target: ~A (~A)\n" f (type-of f))
;;     (if (string-suffix? ".mli" f)
;;         (emit-signature-target outp f)
;;         (if (string-suffix? ".ml" f)
;;             (emit-module-target outp f)
;;             )))

;; FIXME: better name
;; pass root and dirs to process an embedded bazel repo dir

;; suppose foo/bar is a workspace. then just crawling foo/bar from
;; proj root would use the wrong base dir. passing (foo/bar . ".")
;; will treat foo/bar as base dir. to process just foo/bar/a and
;; foo/bar/b, pass (foo/bar . '("a" "b"))
(define* (->bazel root . dirs)
  (format #t "->BAZEL root: ~A dirs: ~A\n" root dirs)
  (if (not (null? dirs))
      (if (not (pair? dirs))
          (error 'bad-arg
                 (format #f "second arg to 'crawl' must be a list of dirs\n"))))

  (if (not (file-exists? ".obazl.d/tmp"))
      (system "mkdir -p .obazl.d/tmp"))
  ;; (let ((codept-args-outp
  ;;        (open-output-file ".obazl.d/tmp/codept.args")))

    ;; if no dirs passed, set root to ./ and dirs to root
  (let* ((root (if (null? dirs) "." root))
         (dirs (if (null? dirs) (list root) dirs))
         (codept-sexp (read-codept-depsfile ".obazl.d/tmp/codept.deps"))
         (codept-alist (cdr codept-sexp)))

      (format #t "Codept: ~A\n" (car codept-alist))

      (for-each
       (lambda (dir)
         (format #t "root ~A, dir ~A\n" root dir)
         (if (char=? #\/ (string-ref dir 0))
             (error 'wrong-type-arg "paths must be relative"))
         (if (not (directory? dir))
             (error 'wrong-type-arg
                    (string-append "Not a directory: " dir)))

         ;; we will recur on subdirs, so we need a new root
         (let recur-dir ((subroot root)
                         (dir dir))
           (if (null? dir)
               (begin
                 ;; this should never happen, we only recur when we have a dir
                 (error 'null-dir (format #f "unexpected null dir\n")))
               (let* ((path
                       (if (equal? subroot ".")
                           dir
                           (string-append subroot "/" dir))
                       )
                      (files ;; first two entries always ".", ".."
                       (cddr (sort! (directory->list path) string<?)))
                      (modules (map file-name->module-name
                                 (filter (lambda (f)
                                           (string-suffix? ".ml" f))
                                         files)))
                      (sigs (map file-name->module-name
                                 (filter (lambda (f)
                                           (string-suffix? ".mli" f))
                                         files))))
                 (if (and (null? modules) (null? sigs))
                     ;; skip BUILD.bazel gen, but still recur on subdirs

                     ;; we already know files contains no ocaml srcs,
                     ;; we just need to recur on the subdirs
                     (for-each (lambda (f)
                                 (let ((fpath (if (equal? path ".")
                                                  f
                                                  (string-append path "/" f))))
                                   (if (not (or (equal? ".." f)
                                                (equal? "." f)
                                                (string-prefix? "." f)
                                                (string-prefix? "bazel-" f)))
                                       (if (directory? fpath)
                                           (recur-dir (if (equal? subroot ".")
                                                          dir
                                                          (string-append subroot "/" dir))
                                                      f)))))
                               files)

                     ;; (emit-codept-args codept-args-outp path files)

                     (let* ((build-file (string-append path "/BUILD.bazel"))
                            (outp
                             (catch #t
                                    (lambda ()
                                      (open-output-file build-file))
                                    (lambda args
                                      (format #t "OPEN ERROR: ~A" build-file)))))
                       (format #t "path: ~A\n" path)
                       ;; (format #t "modules: ~A\n" modules)
                       ;; (format #t "sigs: ~A\n" sigs)
                       ;; (format #t "CRAWLING: ~A\n" path)

                       (emit-buildfile-header outp files modules sigs)
                       (emit-library-target outp
                                            path
                                            modules sigs)

                       (for-each (lambda (f)
                                   (let ((fpath (if (equal? path ".")
                                                    f
                                                    (string-append path "/" f))))
                                     (if (not (or (equal? ".." f)
                                                  (equal? "." f)
                                                  (string-prefix? "." f)
                                                  (string-prefix? "bazel-" f)))
                                         (if (directory? fpath)
                                             (recur-dir (if (equal? subroot ".")
                                                            dir
                                                            (string-append subroot "/" dir))
                                                        f)
                                             (cond
                                              ((string-suffix? ".mli" f)
                                               (emit-signature-target outp path f codept-alist))
                                              ((string-suffix? ".ml" f)
                                               (emit-module-target outp path f sigs codept-alist))
                                              (else
                                               ))))))
                                 files)
                       ;; (format #t "finished dir: ~A\n" path)
                       (close-output-port outp)))))))
       ;; for each dir
       (if (null? dirs) (list ".") dirs))
      ;;(close-output-port codept-args-outp)
      ))

(->bazel ".")


;;(crawl "stdlib")
;; (crawl "." (cons "." (filter directory? (fs-glob->list "*"))))

;; ( (file stdlib/bytes.ml) (deps
;; ((Stdlib Sys) (Stdlib Stdlib) (Stdlib Seq) (Stdlib Int) (Stdlib Char))) )
