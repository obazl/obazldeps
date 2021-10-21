(load "dune_stanza_fields.scm")
(require write.scm)

(define (sym<? s1 s2)
  (let ((x1 (symbol->string s1)) (x2 (symbol->string s2)))
    (string<? x1 x2)))

(define (normalize-stanza-library pkg-path srcfiles stanza)
  ;; (format #t "normalize-stanza-library ~A\n" pkg-path)
  (let* ((stanza-alist (cdr stanza))
         (privname (assoc 'name stanza-alist))

        (s
         (remove
          '()
          (map (lambda (fld-assoc)
                 (case (car fld-assoc)
                   ((name)
                    (normalize-stanza-fld-name
                     pkg-path privname stanza-alist))
                   ;; ((name) (normalize-stanza-fld-name (cadr fld-assoc)))
                   ;; ((public_name)
                   ;;  (normalize-stanza-fld-public_name (cadr fld-assoc)))
                   ((flags) (normalize-stanza-fld-flags fld-assoc))
                   ((library_flags)
                    (let ((lf (normalize-stanza-fld-lib_flags fld-assoc)))
                      ;; (format #t "lib flags: ~A\n" lf)
                      lf))
                   ((libraries) (normalize-stanza-fld-libraries fld-assoc))
                   ((modules)
                    (let-values (((direct indirect)
                                  (normalize-stanza-lib-fld-modules
                                   srcfiles fld-assoc)))
                      ;; (format #t "direct: ~A, indirect ~A\n"
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
                             (result `(:submodules ,indirect)))
                        ;; (format #t "XRESULT ~A: ~A\n" privname result)
                        result)))

                   ((foreign_stubs)
                    (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

                   ((public_name) '())

                   (else fld-assoc)))
               stanza-alist))))

    ;; update global private -> public name table
    (begin
      ;; (format #t "S: ~A\n" s)
      (let* ((private-name (assoc-in '(:name :private) s))
             (public-name  (assoc-in '(:name :public) s)))
        (if (and private-name public-name)
            (begin
              ;; (format #t "writing ~A => ~A\n" private-name public-name)
              (hash-table-set! private-name->public-name
                               (cadr private-name)
                               (cadr public-name)))))
      )

    ;; if 'submodules' fld is missing then add all modules
    (if-let ((mods (assoc :submodules s)))
            (list :library s) ;; (car stanza)
            (list :library
                  (cons (list :submodules
                              (list
                               '(:raw '())
                               `(:direct
                                 ,@(sort!
                                    (srcs->module-names srcfiles '())
                                    sym<?))))
                        s)))))
