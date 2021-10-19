;; (format #t "Loading .obazl.d/scm/dune.scm\n")

(load "alist.scm")
(load "srfi.scm")

;; dune-alist:
;;     ((:nss nss-alist) ...)

;; library stanza => ns-alist
;; ns-alist:
;;       ((:ns ns-namesym) (:path path)
;;        (:submodule-index (m1 m2 ...))
;;        (:submodules smtable)
;;        (:flags (f1 f2 ...))
;;        (:ppx ...)
;;        (:rules ...)
;;        ... one assoc per field ...)
;; multiple library stanzas:
;;  (:nss-alist ((ns-namesym ns-alist) ...))

;; executable stanza =>
;; 'action' stanza:
;; smtable: module-name => module-alist
;; module-alist:
;; ((:mname module-name)
;;  (:flags (f1 f1 ...))
;;  (:files ((:ml  ((:file ml-file) (:deps ...)))
;;           (:mli ((:file mli-file) (:deps ...))))))


(define (pkg-has-ns-archive? dune-pkg-tbl)
  (if-let ((lib (assoc-in '(:stanzas library) (cdr dune-pkg-tbl))))
          (if-let ((wrapped (assoc 'wrapped (cdr lib))))
                  (begin
                    ;; (display (format #f "WRAPPED ~A\n~A"
                    ;;                  dune-pkg-tbl (cadr wrapped)))
                    ;; (newline)
                    ;; FIXME: handle '(wrapped)'
                    (if (equal? 'false  (cadr wrapped))
                        #f
                        #t))
                  (begin
                    ;; (display (format #f "NOWRAPPED ~A" dune-pkg-tbl))
                    ;; (newline)
                    #t))
          #f))

;; (define (pkg-has-stanza? rule dune-pkg-tbl)
;;   ;; (display (format #f "~A" dune-pkg-tbl))
;;   ;; (newline)
;;   (if (assoc-in `(:stanzas ,rule) (cdr dune-pkg-tbl))
;;       #t
;;       #f))

(define (pkg-has-library? dune-pkg-tbl)
  (if-let ((libs (assoc-in+ '(:stanzas library) (cdr dune-pkg-tbl))))
          (begin
            ;; (display (format #f "libs: ~A" libs)) (newline)
            (any (lambda (lib)
                   (if-let ((wrapped (assoc 'wrapped (cdr lib))))
                           ;; FIXME: handle empty '(wrapped)'
                           (if (equal? 'false  (cadr wrapped)) #t #f)
                           (begin
                             ;; (display (format #f "NOWRAP ~A" dune-pkg-tbl))
                             ;; (newline)
                             #f)))
                 libs))
          #f))

(define dunefile-ext-list
  ;; FIXME: parameterize this, in .obazlrc
  (list ".dune.inc" ".dune"))

(define (dunefile? f)
  (display (format #f "DUNEFILE? ~A" f)) (newline)
  (or (string=? "dune" f)
      (any (lambda (sfx) (string-suffix? sfx f))
            dunefile-ext-list)))

(define (-dune-filter lst)
  (display (format #f "-dune-filter ~A" lst)) (newline)
  (cond ((null? lst) #f)
        ((dunefile? (car lst))
         (car lst))
        (else (-dune-filter (cdr lst)))))

(define (process-dune-includes path includes stanzas)
  (format #t "process-dune-includes ~A: ~A\n" path includes)
  (apply append (map (lambda (include)
                      (let ((inc (cadr include)))
                        ;; (format #t "including: ~A\n" inc)
                        (let ((sexps (read-dunefile
                                      path
                                      (if (symbol? inc) (symbol->string inc)
                                          (if (string? inc) inc
                                              (error 'bad-arg'
                                                     "included non-sym, non-string"))))))
                          ;; (format #t "readed sexps: ~A\n" sexps)
                          sexps)))
                    includes)))

  ;; (map (lambda (stanza)
  ;;        (if (eq? 'include (car stanza))
;;            (car (read-dunefile path
;;                           (if (string? (cadr stanza))
;;                               (cadr stanza)
;;                               (if (symbol? (cadr stanza))
;;                                   (symbol->string (cadr stanza))
;;                                   '())))) ;; error
;;            stanza))
;;      stanzas))

(define (handle-read-error path fname args)
  (format #t "HANDLE-READ-ERROR ~A\n" path)
  (let ((msg (caadr args)))
    (display
     (if (string-prefix? "unexpected close paren:" msg)
         ;; may be caused by e.g.
         ;; (run %{bin:tezos-protocol-compiler}  .)
         (begin
           ;; FIXME: search for ' .)'
           (display (format #f "ERROR reading ~A/~A: unexpected close paren.\n  Hint: do not use '.' for current directory, use './' instead.\n"
                            path fname))
           (error 'read-error msg))
         (format #t "ERROR ~A\n" msg)))
    '()))

(define (read-dune-project-file path)
  (let ((dune-project-file (string-append path "/dune-project")))
    (if (file-exists? dune-project-file)
        (let* ((dune-project-port (open-input-file dune-project-file))
               (stanzas (reverse
                         (let recur ((sexp-list '()))
                           ;; (format #t "reading ~A/~A\n" path fname)
                           (let ((next-sexp
                                  (catch 'read-error
                                         (lambda () (read dune-project-port))
                                         (lambda args
                                           (handle-read-error path "dune-project"
                                                              args)))))
                             ;; (format #t "readed dune-project: ~A\n" next-sexp)
                             (if (eof-object? next-sexp)
                                 (begin
                                   (close-input-port dune-project-port)
                                   sexp-list)
                                 (recur (cons next-sexp sexp-list))))))))
              stanzas)
        ;; else no dune-project file
        #f)))

(define (read-dunefile path fname)
  ;; (format #t "read-dunefile ~A/~A\n" path fname)
  (let* (;;(dune-project-stanzas (read-dune-project-file path))
         ;; (_ (if dune-project-stanzas
         ;;        (format #t "dune-project: ~A/dune-project\n ~A\n"
         ;;                path dune-project-stanzas)))
         (dunep (open-input-file (string-append path "/" fname)))
         (stanzas (reverse (let recur ((sexp-list '()))
                             ;; (format #t "reading ~A/~A\n" path fname)
                             (let ((next-sexp
                                    (catch 'read-error
                                           (lambda () (read dunep))
                                           (lambda args
                                             (handle-read-error path fname args)))))
                               ;; (format #t "readed: ~A\n" next-sexp)
                               (if (eof-object? next-sexp)
                                   (begin
                                     (close-input-port dunep)
                                     sexp-list)
                                   (recur (cons next-sexp sexp-list))))))))
    ;; (format #t "READED STANZAS: ~A\n" stanzas)

    ;; now we have a list of stanzas read from a dune file.
    ;; find and read the 'include' fields
    (let ((includes (assoc+ 'include stanzas))
          (stripped (alist-delete '(include) stanzas)))
      ;; (format #t "EMBEDDED INCS: ~A\n" includes)
      (if (not (null? includes))
          (begin
            (format #t "INCS: ~A\n" includes)
            (format #t "STRIPPED: ~A\n" stripped)))
      (if (null? includes)
          ;; (if dune-project-stanzas
          ;;     (cons `(:dune-project ,dune-project-stanzas) stanzas)
              stanzas ;; )
          (let ((included-stanzas
                 (process-dune-includes path includes stanzas)))
            ;; (if dune-project-stanzas
            ;;     (cons `(:dune-project ,dune-project-stanzas)
            ;;           (concatenate included-stanzas stripped))
                (concatenate included-stanzas stripped)))))) ;; )

(define (lib-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :library
     `,@dune-alist
     ;; `(:path ,pfx)
     ;; '(:ns Foo)
     ;; (list :dunefile (string-append pfx "/dune"))
     ;; `(:submodule-index (,m1 ,m2 ,m3))
     ;; '(:srcfiles ())
     ;; '(:flags (f1 f2))
     ;; '(:ppx ())
     )))

(define (exec-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :executable
     dune-alist)))

(define (execs-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :executables
     dune-alist)))

(define (rule-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :rule
     dune-alist)))

(define (test-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :test
     dune-alist)))

(define (include-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :include
     dune-alist)))

(define (alias-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :alias
     dune-alist)))

(define (other-stanza->alist pfx dune-alist)
  (let ((m1 'A) (m2 'B) (m3 'C))
    (list
     :other
     dune-alist)))

(define (pkg->libraries pkg)
  (let ((stanzas (cdr (assoc :stanzas pkg))))
    (assoc+ :library stanzas)))

(define (sort-srcfiles srcfiles)
  (if-let ((srcs (assoc :ocaml (cadr srcfiles))))
          (sort! (cadr srcs) string<?))
          ;; '())

  (if-let ((srcs (assoc :sh (cadr srcfiles))))
          (sort! (cadr srcs) string<?))
          ;; '())

  (if-let ((srcs (assoc :mll (cadr srcfiles))))
          (sort! (cadr srcs) string<?))
          ;; '())

  (if-let ((srcs (assoc :mly (cadr srcfiles))))
          (sort! (cadr srcs) string<?))
          ;; '())
  srcfiles)


  ;; (format #t "sort-srcfiles ~A\n" srcfiles)
  ;; (let* ((ocaml-srcs (if-let ((srcs (assoc :ocaml (cadr srcfiles))))
  ;;                            (sort! (cadr srcs) string<?)
  ;;                            '()))
  ;;        (sh-srcs (if-let ((srcs (assoc :sh (cadr srcfiles))))
  ;;                         (sort! (cadr srcs) string<?)
  ;;                         '()))
  ;;        (mll-srcs (if-let ((srcs (assoc :mll (cadr srcfiles))))
  ;;                         (sort! (cadr srcs) string<?)
  ;;                         '()))
  ;;        (mly-srcs (if-let ((srcs (assoc :mly (cadr srcfiles))))
  ;;                         (sort! (cadr srcs) string<?)
  ;;                         '())))
  ;;   (list
  ;;    (list :ocaml ocaml-srcs))))

(define (normalize-stanzas pkg-path dune-project-stanzas srcfiles stanzas)
  ;; (format #t "normalize-stanzas: ~A\n" stanzas)
  (flush-output-port)
  (let ((normed (map
                 (lambda (stanza)
                   (normalize-dune-stanza
                    pkg-path
                    dune-project-stanzas
                    srcfiles ;; s/b '() ??
                    stanza))
                 (cadr stanzas))))
    ;; normed
    ;; (format #t "normalized stanzas: ~A\n" normed)

    ;; 'executables' normalizes to a list of 'executable' so we flatten
    (let ((result (let recur ((stanzas normed))
                    (if (null? stanzas) '()
                        (begin
                          ;; (format #t "  Stanza: ~A\n" (car stanzas))
                          (if (pair? (caar stanzas))
                              (concatenate (car stanzas)
                                           (recur (cdr stanzas)))
                              (concatenate
                               `(,(car stanzas))
                               (recur (cdr stanzas)))))))))
      ;; (format #t "Renormalized stanzas: ~A\n" result)
      result)
    ))

(define (normalize-pkg-tbl pkg-tbl)
  ;; (format #t "NORMALIZE-PKG-TBL ct: ~A\n" (hash-table-entries pkg-tbl))
  (let ((pt (for-each (lambda (pkg-kv)
              ;; (format #t "PKG: ~A\n" (car pkg-kv))
              (let* ((pkg-path (car pkg-kv))
                     (pkg-alist (cdr pkg-kv))
                     ;; (pkg-path (if-let ((pp (assoc :pkg-path pkg-alist)))
                     ;;                   (cadr pp)
                     ;;                   ""))
                     (dune-project-stanzas (read-dune-project-file pkg-path))
                     (srcfiles (if-let ((s (assoc
                                            :srcfiles ;;  :ocaml)
                                            (cdr pkg-kv))))
                                       s;;(cadr s)
                                       '())))
                (if-let ((stanzas-lst (assoc :stanzas pkg-alist)))
                        (let* ((stanzas (normalize-stanzas
                                         pkg-path
                                         dune-project-stanzas
                                         srcfiles stanzas-lst))
                               ;; FIXME: update-in! pkg?
                               (new-pkg (if (null? srcfiles)
                                            (list
                                             (list :pkg-path pkg-path)
                                             (list :dune-project
                                                   dune-project-stanzas)
                                             (list :stanzas stanzas))
                                            ;; else has srcfiles
                                            (list
                                             (list :pkg-path pkg-path)
                                             (list :dune-project
                                                   dune-project-stanzas)
                                             (sort-srcfiles srcfiles)
                                             (list :stanzas stanzas)))))
                          (hash-table-set! pkg-tbl
                                           pkg-path
                                           new-pkg))
                        ;; else no stanzas == no dunefile, no update
                        ;; (format #t "NO STANZAS ~A\n" pkg-path)
                        )))
                      pkg-tbl)))
  ;; return updated pkg table
    pkg-tbl))

(define (pkg-stanzas->public-names-table! stanzas pname-tbl)
  ;; (format #t "pkg-stanzas->public-names-table! ~A\n" stanzas)
  (for-each
   (lambda (stanza)
     ;; stanza:: (<stanza-type> <alist>)
     (format #t "STANZA: ~A\n" (car stanza))
     (let* ((stanza-alist (cadr stanza)))
       ;; stanza-alist: (<assoc> <assoc> ...)
       (case (car stanza)
         ((:library)
          (let ((modname (assoc-in '(:name :module) stanza-alist))
                (privname (assoc-in '(:name :private) stanza-alist))
                (pubname (assoc :public_name stanza-alist)))
            (format #t "    modname: ~A\n" modname)
            (format #t "    privname: ~A\n" privname)
            (format #t "    pubname: ~A\n" pubname)
            (if (and privname pubname)
                (begin
                  (hash-table-set! pname-tbl
                                   (cadr privname)
                                   (cadr pubname))
                  (hash-table-set! pname-tbl
                                   (cadr pubname)
                                   (cadr privname))
                  ))
            (if (and pubname modname)
                (begin
                  (hash-table-set! pname-tbl
                                   (cadr pubname)
                                   (cadr modname))
                  (hash-table-set! pname-tbl
                                   (cadr modname)
                                   (cadr pubname))))))
         ((:executable)
          (begin
            (format #t "executable->pnt: ~A\n" stanza)
            ))
         ((rule) ) ;; skip
       (else
        ;; skip
        ))))
     (cadr stanzas)))

(define (libdeps->public-names-table dune-pkg-tbls)
  (let ((pname-tbl (make-hash-table)))
    (for-each (lambda (pkg-tbl)
                (format #t "PKG-TBL: ~A\n" (car pkg-tbl))
                ;; pkg-tbl: (<dir-path> <pkg hash-tbl>) i.e. assoc pair
                (for-each (lambda (pkg)
                            ;; pkg: (<pkg-path> <pkg-list>)
                            (format #t "PKG: ~A\n" (car pkg))
                            (if-let ((stanzas (assoc :stanzas (cdr pkg))))

                                    (pkg-stanzas->public-names-table!
                                     stanzas pname-tbl)

                                    ;; else skip - pkg w/o dune stanzas
                                    ))
                          (cadr pkg-tbl)))
              dune-pkg-tbls)
    pname-tbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-modtbl-null stanza)
  stanza)

(define (update-modtbl-executable path modules-tbl stanza)
  (format #t "update-modtbl-executable: ~A\n ~A\n" path stanza)
  ;; (assoc :name (cadr stanza)))
  (let* ((stanza (cadr stanza))
         (module-name (assoc-in '(:name :module) stanza))
         (private-name (assoc-in '(:name :private) stanza))
         (pubname (assoc-in '(:name :public) stanza))
         (pubname (assoc :public_name stanza))
         (key (if pubname (cadr pubname) (cadr private-name))))
    ;; stanza: (<assoc> <assoc> ...)
    (format #t "    modname: ~A\n" module-name)
    (format #t "    privname: ~A\n" private-name)
    (format #t "    pubname: ~A\n" pubname)
    (format #t "    key: ~A\n" key)
    (hash-table-set! modules-tbl
                     key
                     `((:exe)
                       (:path ,path)))))

(define (update-modtbl-executables path modules-tbl stanza)
  ;; (format #t "update-modtbl-executables: ~A\n" (assoc :names (cadr stanza)))
  (let* ((stanza (cadr stanza))
         (names (assoc :names stanza))
         (pnames (assoc :public_names stanza)))
    ;; stanza: (<assoc> <assoc> ...)
    ;; (format #t "dune->modules-tbl NAMES: ~A\n" names)
    ;; (format #t "dune->modules-tbl PNAMES: ~A\n" pnames)
    (for-each (lambda (name)
                (if-let ((entry (hash-table-ref modules-tbl name)))
                        ;; update
                        (hash-table-set! modules-tbl
                                         name
                                         ;;stanza
                                         `((:exe)
                                           (:path ,path)))
                        (hash-table-set! modules-tbl
                                         name
                                         ;;stanza
                                         `((:exe)
                                           (:path ,path)))
                        ))
              (cadr names))))

(define (update-modtbl-library modules-tbl path stanza)
  ;; (format #t "update-modtbl-library: ~A\n" (assoc :name (cadr stanza)))
    ;; stanza: (<assoc> <assoc> ...)
  (let* ((stanza (cadr stanza))
         (name (assoc :name stanza))
         (raw-name (cadr (assoc-in '(:name :private) stanza)))
         (mod-name (cadr (assoc-in '(:name :module) stanza)))
         (pub-name (if-let ((pname (assoc :public_name stanza)))
                          (cadr pname) #f))
         (wrapped? (if-let ((wrapped (assoc :wrapped stanza)))
                           (if (equal? wrapped 'false) #f #t)
                           #t))
         (opts (assoc :opts stanza))
         (submodules (assoc :submodules stanza))
         (deps    (assoc :deps    stanza)))
    (if-let ((entry (hash-table-ref modules-tbl mod-name)))
            (begin
              (format #t "DUP: ~A\n" mod-name)
              (hash-table-set! modules-tbl mod-name
                               (append entry '(:dup))))
            ;; else new entry
            (hash-table-set! modules-tbl mod-name
                             (list
                              `(:name
                                ((:private    . ,raw-name)
                                 (:module . ,mod-name)
                                 (:public . ,pub-name)))
                              `(:path ,path)
                              '(:aggregate)
                              (if-let ((wrapped (assoc 'wrapped stanza)))
                                      (if (equal? 'false (cadr wrapped))
                                          '(:library)
                                          '(:ns-archive))
                                      '(:ns-archive))
                              opts
                              `(:submodules ,@(cdr submodules))
                              deps)))))

(define (dune->modules-tbl pkg-tbls-list codept-sexp)
  (let ((modules-tbl (make-hash-table)))
    (for-each
     (lambda (pkg-tbl)
       ;; (format #t "PKG-TBL: ~A\n" pkg-tbl)
       ;; pkg-tbl: (<dir-path> <pkg hash-tbl>) i.e. assoc pair
       (for-each
        (lambda (pkg)
          ;; (format #t "PKG path: ~A\n" (car pkg))
          ;; pkg: (<pkg-path> list of assocs)
          ;; (cdr pkg) : pkg-alist
          (if-let ((stanzas (assoc :stanzas (cdr pkg))))
                  (begin
                    ;; (format #t "STANZAS: ~A\n" "X") ;stanzas)
                    (for-each
                     (lambda (stanza)
                       ;; stanza-pair, e.g. (library <alist>)
                       (case (car stanza)
                         ((alias) (update-modtbl-null stanza))
                         ((copy_files) (update-modtbl-null stanza))
                         ((data_only_dirs) (update-modtbl-null stanza))
                         ((env) (update-modtbl-null stanza))
                         ((:executable) (update-modtbl-executable
                                         (car pkg) ;; path
                                         modules-tbl stanza))
                         ((:executables) (update-modtbl-executables
                                          (car pkg) ;; path
                                          modules-tbl stanza))
                         ((install) (update-modtbl-null stanza))
                         ((:library) (update-modtbl-library
                                     modules-tbl
                                     (car pkg) ;; pkg-path
                                     stanza))
                         ((ocamllex) (update-modtbl-null stanza))
                         ((rule) (update-modtbl-null stanza))
                         ((test) (update-modtbl-null stanza))
                         ((tests) (update-modtbl-null stanza))

                         ((:genrule)) ;; skip
                         ((:write-file)) ;; skip
                         (else
                          (format #t "dune->modules-tbl unhandled: ~A\n" stanza))))
                     (cadr stanzas)))
                  ;; else skip - pkg w/o dune stanzas
                  ;;(format #t "NO STANZAS: ~A\n" (car pkg))
                  ))
        (cadr pkg-tbl)))
     pkg-tbls-list)
    modules-tbl))

(define (dump-stanzas pkgs-tbl)
  (for-each (lambda (kv)
              ;; (display (format #f "fs-path: ~A" (car kv)))
              ;; (newline)
              ;; (display (format #f "k: ~A, v: ~A" (car kv) (cdr kv)))
              ;; (newline)
              ;; (display (format #f " stanzas: ~A"
              ;;                  (assoc :stanzas (cdr kv))))
              ;; (newline)
              (let ((stanzas (cdr (assoc :stanzas (cdr kv)))))
                (display (format #f " libraries: ~A"
                                 (assoc+ :library stanzas)))
                (newline)
                (display (format #f " executable: ~A"
                                 (assoc+ :executable stanzas)))
                (newline)
                (display (format #f " executables: ~A"
                                 (assoc+ :executables stanzas)))
                (newline)
                (display (format #f " alias: ~A"
                                 (assoc+ :alias stanzas)))
                (newline)))
            pkgs-tbl)
  '())

