(display "loading @ocamlark//scm/ocamlark.scm")
(newline)

(define root "")
(define rootlen 0)

;; these are direct from the codeps sexp
(define version '())
(define dependencies '())
(define locals '())
(define unknowns '())

;; we construct these
;; pkgs-map: pkg-path => pkg-list
;; pkg-list:  (pkg-abs-path pkg-modules-map)
;; pkg-modules-map: module-name => module-assoc
;; module-assoc: ((:ml (filename, deplist)) (:mli (filename, deplist)))
;; deplist: list of (pkg-path, module-name)

(define pkgs-map (make-hash-table))
(define modules-table (make-hash-table))

(define-macro* (if-let bindings true false)
  (let* ((binding-list (if (and (pair? bindings) (symbol? (car bindings)))
			   (list bindings)
			   bindings))
	 (variables (map car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
	   ,true
	   ,false))))

;; string-capitalize from s7test.scm
(define (cl-string x)
  (if (string? x) x
      (if (char? x)
	  (string x)
	  (if (symbol? x)
	      (symbol->string x)
	      (error 'wrong-type-arg "string ~A?" x)))))

(define* (nstring-capitalize str-1 (start 0) end)
  (define (alpha? c)
    (or (char-alphabetic? c)
	(char-numeric? c)))
  (let ((str (cl-string str-1)))
    (let ((nd (if (number? end) end (length str))))
      (do ((i start (+ i 1)))
	  ((= i nd) str)
	(if (alpha? (str i))
	    (if (or (= i 0)
		    (not (alpha? (str (- i 1)))))
		(set! (str i) (char-upcase (str i)))
		(set! (str i) (char-downcase (str i)))))))))

(define* (string-capitalize str-1 (start 0) end)
  (let ((str (cl-string str-1)))
    (nstring-capitalize (copy str) start end)))

(define (ocaml_src? s)
  (let* ((slen (string-length s))
         (mli (substring s (- slen 4) slen))
         (ml  (substring s (- slen 3) slen)))
    (cond
     ((equal? ".mli" mli) #t)
     ((equal? ".ml" ml) #t)
     (else #f))))

(define (dirent->module-name dirent)
  ;; (display (format #f "dirent->module-name ~S" dirent)) (newline)
  (let* ((dotpos (string-position "." dirent))
         (mname (string-capitalize (substring dirent 0 dotpos)))
         )
    ;; (display (format #f "s: ~S, m: ~S" dirent mname)) (newline)
    mname))

(define (update-pkgs-map pkg dirent)
  (display (format #f "update-pkgs-map pkg ~A" pkg)) (newline)
  (let ((modules-tbl (hash-table-ref pkgs-map pkg))
        (module (dirent->module-name dirent)))
    ;; (display (format #f "module ~A, file ~S" module dirent)) (newline)
    (if modules-tbl
        (begin
          ;; (display (format #f "FOUND modules-tbl ~A for module: ~A"
          ;;                  modules-tbl
          ;;                  (hash-table? modules-tbl)))
          ;; (newline)
          (let ((mfiles (hash-table-ref modules-tbl module)))
            (if mfiles
                (begin
                  ;; (display (format #f "adding file to mfiles ~A" mfiles))
                  ;; (newline)
                  (hash-table-set! modules-tbl module
                                   (append mfiles (list dirent)))
                  (hash-table-set! pkgs-map pkg modules-tbl)
                  )
                (begin
                  ;; (display (format #f "adding mfiles for ~A" module))
                  ;; (newline)
                  (hash-table-set! modules-tbl module (list dirent))
                  (hash-table-set! pkgs-map pkg modules-tbl)))))
        (begin
          ;; (display (format #f "INITIAL PKG MODULES-TBL: ~S" pkg)) (newline)
          (hash-table-set! pkgs-map
                           pkg
                           (hash-table module (list dirent)))))))

(define (dir->srclist pfx dir)
  (display (format #f "dir->srclist: (~A) ~A" pfx dir)) (newline)
  (let* ((path (string-append pfx "/" dir))
         (pkg (substring path rootlen (string-length path))))
    ;; (display (format #f "path: ~A" path)) (newline)
    (for-each (lambda (dirent)
                ;; (display (format #f "d: ~A" dirent)) (newline)
                (let ((dirent-path (string-append path "/" dirent)))
                  (if (not (equal? dirent ".."))
                      (if (not (equal? dirent "."))
                          (begin
                            (if (not (directory? dirent-path))
                                (if (ocaml_src? dirent)
                                    (update-pkgs-map pkg dirent)
                                    ;; (begin
                                    ;;   (display (format #f "OMIT ~S" dirent))
                                    ;;   (newline))
                                    )
                                (if (directory? dirent-path)
                                    (dir->srclist path dirent))))))))
              (directory->list path)))
  )

(define (fileseq rootdir subdirs)
  (display (format #f "running fileseq on ~S" rootdir)) (newline)
  (display (format #f "subdirs: ~A" subdirs)) (newline)

  ;; (display (format #f "string-capitalize test: ~S"
  ;;                  (string-capitalize "foo")))
  ;; (newline)

  (set! root rootdir)
  (set! rootlen (+ 1 (string-length rootdir)))
  (display (format #f "root: ~S len: ~D" root rootlen)) (newline)

  (for-each (lambda (subdir)
              (if (directory? subdir)
                  (if (not (equal? subdir ".."))
                      (if (not (equal? subdir "."))
                          (dir->srclist rootdir subdir)))))
            subdirs)

  ;; (display pkgs-map)
  ;; (display (format #f "entry count: ~D"
  ;;                  (hash-table-entries pkgs-map))) (newline)
  ;; (newline)

  (emit-codept-args)

  "fileseq ok")

(define (emit-codept-args)
  (display "emit-codept-args") (newline)
  (call-with-output-file ".obazl.d/codept.args"
    (lambda (p)
      (for-each (lambda (pkg-entry)
              (display (format #f "~A[" (car pkg-entry)) p)
              (for-each (lambda (item)
                          (for-each (lambda (fname)
                                      (display (format #f "~A/~A,"
                                                       (car pkg-entry) fname) p))
                                    (cdr item)))
                        (cdr pkg-entry))
              (display "]" p)
              (newline p))
              ;; (newline p))
           pkgs-map)))
  )

(define (log-stats codept-sexp)
  (display (format #f "deps count: ~D" (length codept-sexp)))
  (newline)

  (set! version (car codept-sexp))
  (display (format #f "version: ~A" version))
  (newline)

  (set! dependencies (cadr (cadr codept-sexp)))
  ;; (display (format #f "deps: ~A" dependencies))
  ;; (newline)
  (display (format #f "dependencies count: ~A" (length dependencies)))
  (newline)

  (set! locals (cadr (caddr codept-sexp)))
  ;; (display (format #f "locals: ~A" locals))
  ;; (newline)
  (display (format #f "locals count: ~A" (length locals)))
  (newline)

  (set! unknowns (cadr (cadddr codept-sexp)))
  ;; (display (format #f "unknowns: ~A" unknowns))
  ;; (newline)
  (display (format #f "unknowns count: ~A" (length unknowns)))
  (newline)
  )

(define (file->deps f)
  ;; search deps table for (file f) entry
  ;; (display (format #f "file->deps for ~A" f))
  ;; (newline)
  (let* ((deps (assoc (list 'file f) dependencies))
         (ds (if (pair? (cdr deps))
                 (cadr (cadr deps))
                 '())))
    ;; (display (format #f "  deps list: ~A" ds))
    ;; (newline)
    ds
    )
  )

(define (make-deps-table depsfile)
  (display "make-deps-table") (newline) (newline)
  (let* ((p (open-input-file depsfile))
         (codept-sexp (read p)))
    (close-input-port p)

    (set! version (car codept-sexp))
    (display (format #f "version: ~A" version)) (newline) (newline)

    (set! dependencies (cadr (cadr codept-sexp)))
    (display (format #f "deps: ~A" dependencies)) (newline) (newline)

    (set! locals (cadr (caddr codept-sexp)))
    (display (format #f "locals: ~A" locals)) (newline) (newline)

    (set! unknowns (cadr (cadddr codept-sexp)))
    (display (format #f "unknowns: ~A" unknowns)) (newline) (newline)

    (fill! pkgs-map #f)
    ;; locals: (((module (Fuzz)) (ml fuzz/fuzz.ml))...) ...)
    (for-each (lambda (dep)
                (let* ((m (cadar dep))  ;; m := (Fuzz)
                       ;; (context (car m)) ;; ?? Fuzz
                       (module-name (car m))  ;; Fuzz
                       (files (cdr dep)) ;; ((ml fuzz/fuzz.ml) (mli ...))

                       (mli-file (if-let (mlx (assq 'mli files))
                                    (cadr mlx)))
                       (mli-dirname (if mli-file (dirname mli-file) #f))
                       (mli-deps (if mli-file (file->deps mli-file) #f))

                       (ml-file (if-let (mlx (assq 'ml files))
                                        (cadr mlx)))
                       (ml-dirname (if ml-file (dirname ml-file) #f))
                       (ml-deps (if ml-file (file->deps ml-file) #f))

                       ;; NB we don't have to put filedeps in tbl,
                       ;; we can just call file->deps as needed
                       ;; file-deps combines ml, mli entries as list
                       (module-assoc (if mli-file
                                      (if (null? mli-deps)
                                          (list :mli mli-file)
                                          (list :mli mli-file mli-deps))
                                      #f))
                       (_ (begin (display (format #f "MLI ~A" module-assoc))
                                 (newline)))
                       (module-assoc (if module-assoc
                                         (if ml-file
                                             (if ml-deps
                                                 (list module-assoc
                                                       (list :ml ml-file ml-deps))
                                                 (list module-assoc
                                                       (list :ml ml-file)))
                                             module-assoc)
                                         (if ml-deps
                                             (list
                                              :ml ml-file ml-deps)
                                             '())))
                       (_ (begin (display (format #f "ML ~A" module-assoc))
                                 (newline)))
                       ;; either path will do
                       (pkg-path (if ml-dirname ml-dirname mli-dirname))
                       )
                  (display (format #f "pkgs-map: ~A" pkgs-map)) (newline)
                  (let ((pkg-list (hash-table-ref pkgs-map pkg-path)))
                    (if pkg-list
                        (let ((pkg-modules-map (cadr pkg-list)))
                          ;; does pkg-modules-map contain this module?
                          (if (not (hash-table-ref pkg-modules-map
                                                   module-name))
                              (hash-table-set!
                               pkgs-map pkg-path
                               (list
                                (car pkg-list)
                                (begin
                                  (hash-table-set!
                                   pkg-modules-map module-name
                                   module-assoc)
                                  pkg-modules-map)))))
                        ;; pkg-path not in pkgs-map
                        (hash-table-set! pkgs-map
                                         pkg-path
                                         (list
                                          ;;(realpath pkg-path)
                                          pkg-path
                                          (let ((ht (make-hash-table)))
                                            (begin (hash-table-set!
                                                    ht module-name
                                                    module-assoc)
                                                   ht))))))))

                  ;; (newline) (newline)
                  ;; (display (format #f "DEP: ~A" dep))
                  ;; (newline)
                  ;; (display (format #f "files: ~A" files))
                  ;; (newline)
                  ;; (display (format #f "PKG-MAP for PKG ~A: ~A" pkg pkg-map))
                  ;; (newline)
                  ;; (if pkg-map
                  ;;     (if-let ((file-list
                  ;;               (hash-table-ref pkg-map module)))
                  ;;             (begin
                  ;;               (hash-table-set! pkg-map module
                  ;;                                (append file-list
                  ;;                                        (list file-deps))))
                  ;;             ;; module not in pkg-map
                  ;;             (begin
                  ;;               (hash-table-set! pkg-map module
                  ;;                                file-deps)))
                  ;;     ;; pkg not in pkgs-map
                  ;;     (hash-table-set! pkgs-map
                  ;;                      pkg
                  ;;                      (hash-table module file-deps)))
                      ;; module key not in pkgs-map
                  ;; NB: context == capitalize(mlx-dirname)
                  ;; (codept capitalizes the path to make it a module name)
                  ;; (display (format #f "ml: ~A" ml))
                  ;; (newline)
                  ;; (display (format #f "pkg: ~A" pkg))
                  ;; (newline)
                  ;; (display (format #f "ml dirname: ~A" ml-dirname))
                  ;; (newline)

;;                   (display (format #f "local module: ~S, context: ~A, pkg: //
;; ~A"
;;                                    module context pkg))
;;                   (newline)
;;                   ;; (display (format #f "  files: ~A" files))
;;                   ;; (newline)
;;                   (display (format #f "  mli: ~A" mli))
;;                   (newline)
;;                   (display (format #f "  ml: ~A" ml))
;;                   (newline)
                  ;; ))
              locals)

    (display (format #f "pkgs-map ~A" pkgs-map)) (newline)

    ;; (let ((keys (map (lambda (e) (car (values e))) modules-table)))
    ;;   (for-each (lambda (key)
    ;;               (let ((val (hash-table-ref modules-table key)))
    ;;                 (display (format #f "~A: ~D" key (length val)))
    ;;                 (newline)))
    ;;             (sort! keys (lambda (a b)
    ;;                           (string<? (symbol->string a)
    ;;                                     (symbol->string b))))))
    ;; test
    ;; (display (format #f "'Fuzz count ~A: ~A"
    ;;                    (length (hash-table-ref modules-table 'Fuzz))
    ;;                    (hash-table-ref modules-table 'Fuzz)))
    ;;   (newline)
    ;;   (for-each (lambda (pkg)
    ;;               (display (format #f "//~A:~A deps: ~A" pkg "Fuzz"
    ;;                                (hash-table-ref
    ;;                                 (hash-table-ref pkgs-map pkg)
    ;;                                 'Fuzz)))
    ;;               (newline))
    ;;             (hash-table-ref modules-table 'Fuzz))

      (newline)
      ;; (display (format #f "modules-table keys: ~A"
      ;; )))
    ;; (display (format #f "modules table: ~A" modules-table))
    ;; (newline)
    ;; (display (format #f "modules table count: ~D"
    ;;                  (hash-table-entries modules-table)))
    (newline)
    "ok")
)

;; called by ocamlark
(define (ocamlark-handler depsfile)
  (display (format #f "running ocamlark_handler on: ~S" depsfile))
  (newline)

  ;; codept should be on path, if opam switch is properly set
  ;; otherwise, `opam var codept:bin` returns path to executable dir
  ;; (let ((cmd
  ;;        (format #f "codept -sexp -k -args .obazl.d/codept.args 1> ~S 2> .obazl.d/codept.log" depsfile)))
  ;;   (display "running codept") (newline)
  ;;   (system cmd))

  (make-deps-table depsfile)

  ;; test
  (let ((x (file->deps (symbol "lib_test/test_runner.ml"))))
    (display (format #f "deps for lib_test/test_runner.ml: ~A" x))
    (newline))

  (display (format #f "pkgs-map entry for 'etc': ~A"
                   (hash-table-ref pkgs-map "etc")))
  (newline)
  "ok")
