;; lookup_tables.scm

(define (update-modtbl-null stanza)
  stanza)

(define (make-label path submodule)
  `(:label
    ,(string-append
      "//"
      (if (symbol? path) (symbol->string path) path)
      ":"
      (if (symbol? submodule) (symbol->string submodule) submodule))))

(define (make-ns-label path ns submodule)
  `(:label
    ((:module
    ,(string-append
      "//"
      (if (symbol? path) (symbol->string path) path)
      ":"
      (if (symbol? submodule) (symbol->string submodule) submodule)))
     (:ns
      ,(string-append
        "//"
        (if (symbol? path) (symbol->string path) path)
        ":"
        (if (symbol? ns) (symbol->string ns) ns))))))

(define (update-modtbl-executable modules-tbl path stanza)
  ;; (format #t "update-modtbl-executable: ~A\n ~A\n" path stanza)
  ;; (assoc :name (cadr stanza)))
  (let* ((stanza-alist (cadr stanza))
         (privname (assoc-in '(:name :private) stanza-alist))
         (pubname (assoc-in '(:name :public) stanza-alist))
         (key (if pubname (cadr pubname) (cadr privname)))
         (modules (assoc :modules stanza-alist)))
    ;; stanza-alist: (<assoc> <assoc> ...)
    ;; (format #t "    privname: ~A\n" private-name)
    ;; (format #t "    pubname: ~A\n" pubname)
    ;; (format #t "    key: ~A\n" key)
    (hash-table-set! modules-tbl
                     key
                     `((:target ,key)
                       (:exe)
                       (:dir ,path)))

    (if modules
        (begin
          (if-let ((directs (assoc :direct (cadr modules))))
                  (for-each (lambda (module)
                              (hash-table-set! modules-tbl module
                                               `((:module ,module)
                                                 ,(make-label
                                                   path module)
                                                 (:direct)
                                                 (:dir ,path))))
                            (cdr directs)))
          (if-let ((indirects (assoc :indirect (cadr modules))))
                  (for-each (lambda (module)
                              (hash-table-set! modules-tbl module
                                               `((:module ,module)
                                                 ,(make-label
                                                   path module)
                                                 (:indirect)
                                                 (:dir ,path))))
                            (cdr indirects)))))
    ))

;; one entry for aggregate, and one for each submodule
(define (update-modtbl-library modules-tbl path stanza)
  ;; (format #t "update-modtbl-library: ~A\n" path)
  ;; (format #t "    stanza: ~A\n" stanza)
    ;; stanza: (<assoc> <assoc> ...)
  (let* ((stanza-alist (cadr stanza))
         ;; (name (assoc :name stanza-alist))
         (privname (cadr (assoc-in '(:name :private) stanza-alist)))
         (pubname (if-let ((pname (assoc-in '(:name :public) stanza-alist)))
                          (cadr pname) #f))
         (modname (normalize-module-name privname))
         (wrapped? (if-let ((wrapped (assoc :wrapped stanza-alist)))
                           (if (equal? wrapped 'false) #f #t)
                           #t))
         (opts (assoc :opts stanza-alist))
         (submodules (assoc :submodules stanza-alist))
         (deps    (assoc :deps    stanza-alist)))

    ;; (format #t "aggregate: ~A\n" pubname)
    ;; (format #t "submodules: ~A\n" submodules)
    ;; aggregate key: pubname
    (if-let ((entry (hash-table-ref modules-tbl pubname)))
            (begin
              (format #t "DUP: ~A\n" pubname)
              (hash-table-set! modules-tbl pubname
                               (append entry '(:dup))))
            ;; else new entry
            (hash-table-set! modules-tbl pubname
                             (list
                              `(:name
                                ((:public  . ,pubname)
                                 (:private . ,privname)
                                 (:module  . ,modname)))
                              `(:dir ,path)
                              '(:aggregate)
                              (if-let ((wrapped (assoc 'wrapped stanza-alist)))
                                      (if (equal? 'false (cadr wrapped))
                                          '(:library)
                                          '(:ns-archive))
                                      '(:ns-archive))
                              opts
                              `(:submodules ,@(cdr submodules))
                              deps)))
    (if submodules
        (begin
          (if-let ((directs (assoc :direct (cadr submodules))))
                  (for-each (lambda (submodule)
                              (hash-table-set! modules-tbl submodule
                                               `((:module ,submodule)
                                                 ;; label is for ns!
                                                 ,(make-ns-label
                                                  path pubname submodule)
                                                 (:ns ,modname)
                                                 ;; (:srcfile)
                                                 (:dir ,path))))
                            (cdr directs)))
          (if-let ((indirects (assoc :indirect (cadr submodules))))
                  (for-each (lambda (submodule)
                              (hash-table-set! modules-tbl submodule
                                               `((:module ,submodule)
                                                 ,(make-ns-label
                                                  path pubname submodule)
                                                 (:ns ,modname)
                                                 (:generated)
                                                 (:dir ,path))))
                            (cdr indirects)))))
    ))

(define (dune->modules-tbl pkg-tbls-list) ;; codept-sexp)
  (let ((modules-tbl (make-hash-table)))
    (for-each
     (lambda (pkg-tbl)
       (newline)
       ;; (format #t "PKG-TBL: ~A\n" (car pkg-tbl))
       ;; pkg-tbl: (<dir-path> <pkg hash-tbl>) i.e. assoc pair
       (for-each
        (lambda (pkg)
          (newline)
          ;; (format #t "PKG path: ~A\n" (car pkg))
          ;; pkg: (<pkg-path> list of assocs)
          ;; (cdr pkg) : pkg-alist

          ;; add aggregates and their components
          (if-let ((stanzas (assoc :stanzas (cdr pkg))))
                  (begin
                    ;; (format #t "STANZAS: ~A\n" "X") ;stanzas)
                    (for-each
                     (lambda (stanza)
                       ;; (format #t "STANZA: ~A\n" stanza)
                       ;; stanza-pair, e.g. (library <alist>)
                       (case (car stanza)
                         ;; ((alias) (update-modtbl-null stanza))
                         ;; ((copy_files) (update-modtbl-null stanza))
                         ;; ((data_only_dirs) (update-modtbl-null stanza))
                         ;; ((env) (update-modtbl-null stanza))

                         ((:executable) (update-modtbl-executable
                                         modules-tbl
                                         (car pkg)
                                         stanza))

                         ;; executables have been rewritten as executable
                         ;; ((:executables) (update-modtbl-executables
                         ;;                  (car pkg) modules-tbl stanza))

                         ((install) (update-modtbl-null stanza))
                         ((:library) (update-modtbl-library
                                     modules-tbl
                                     (car pkg) ;; pkg-path
                                     stanza))
                         ((ocamllex) (update-modtbl-null stanza))
                         ((rule) (update-modtbl-null stanza))
                         ((test) (update-modtbl-null stanza))
                         ((tests) (update-modtbl-null stanza))

                         ;; ((:genrule)) ;; skip
                         ;; ((:write-file)) ;; skip
                         (else
                          (format #t "dune->modules-tbl unhandled: ~A\n"
                                  (car stanza)))))
                     (cadr stanzas)))
                  ;; else skip - pkg w/o dune stanzas
                  ;;(format #t "NO STANZAS: ~A\n" (car pkg))
                  )

          ;; for each file: look it up in modules-tbl
          ;; if found, then it was added as a component of an aggregate
          ;; if not found, add it as a stand-alone
          (if-let ((srcfiles (assoc-in '(:srcfiles :ocaml) (cdr pkg))))
                  ;; (format #t "SRCFILES: ~A\n" srcfiles)
                  (for-each (lambda (file)
                              ;;(format #t "SRCFILE: ~A\n" file)
                              (begin))
                            (cadr srcfiles))
                  )
          )
        (cadr pkg-tbl)))
     pkg-tbls-list)
    modules-tbl))

(define (xdune->modules-tbl pkg-tbls-list codept-sexp)
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
                       ;; (format #t "STANZA: ~A\n" stanza)
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
                          (format #t "dune->modules-tbl unhandled: ~A\n"
                                  (car stanza)))))
                     (cadr stanzas)))
                  ;; else skip - pkg w/o dune stanzas
                  ;;(format #t "NO STANZAS: ~A\n" (car pkg))
                  ))
        (cadr pkg-tbl)))
     pkg-tbls-list)
    modules-tbl))

