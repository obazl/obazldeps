(display "loading @camlark//scm/camlark.scm")
(newline)

(load "codept.scm")
(load "dune.scm")
(load "utils.scm")

(define root "")
(define rootlen 0)

;; these are direct from the codeps sexp
(define version '())
(define dependencies '())
(define locals '())
(define unknowns '())

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

;; called by ocamlark
(define (camlark-handler depsfile)
  (display (format #f "running camlark_handler on: ~S" depsfile))
  (newline)

  ;; codept should be on path, if opam switch is properly set
  ;; otherwise, `opam var codept:bin` returns path to executable dir
  ;; (let ((cmd
  ;;        (format #f "codept -sexp -k -args .obazl.d/codept.args 1> ~S 2> .obazl.d/codept.log" depsfile)))
  ;;   (display "running codept") (newline)
  ;;   (system cmd))

  (load "codept.scm")

  (make-deps-table depsfile)

  ;; (load "dune.scm")
  ;; (import-dunefile "lib_re")

  ;; test
  ;; (let ((x (file->deps (symbol "lib_test/test_runner.ml"))))
  ;;   (display (format #f "deps for lib_test/test_runner.ml: ~A" x))
  ;;   (newline))

  ;; (display (format #f "pkgs-map entry for 'etc': ~A"
  ;;                  (hash-table-ref pkgs-map "etc")))
  ;; (newline)

  ;;"ok"
  )
