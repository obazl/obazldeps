;; (format #t "Loading .obazl.d/scm/dune.scm\n")

(load "alist.scm")
(load "dune_normalize.scm")
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

(define (pkg-namespaced? dune-pkg-tbl)
  (any (lambda (s)
         (if (assoc-in '(:library :namespaced) (list s)) #t #f))
       (cadr (assoc :stanzas (cdr dune-pkg-tbl)))))


;; dune always makes an archive from 'library' stanza,
;; but may or may not namespace it, depending on 'unwrapped' fld.
(define (pkg-has-archive? dune-pkg-tbl)
  (assoc-in '(:stanzas :library) (cdr dune-pkg-tbl)))

;; unwrapped 'library' stanza
(define (pkg-has-library? dune-pkg-tbl)
  (if-let ((libs (assoc-in+ '(:stanzas :library) (cdr dune-pkg-tbl))))
          (any (lambda (lib)
                 (if-let ((wrapped (assoc 'wrapped (cdr lib))))
                         ;; FIXME: handle empty '(wrapped)'
                         (if (equal? 'false  (cadr wrapped))
                             #t
                             #f)
                         #f))
               libs)
          #f))

(define (pkg-has-signature? dune-pkg-tbl)
  (if-let ((srcs (assoc-in '(:srcfiles :ocaml :static) (cdr dune-pkg-tbl))))
          (any (lambda (srcfile)
                   (string-suffix? ".mli" srcfile))
                 (cadr srcs))
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
  ;; (format #t "process-dune-includes ~A: ~A\n" path includes)
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
                             ;; if sexp == (include ...) then read it in
                             (let ((next-sexp
                                    (catch 'read-error
                                           ;; where the read happens:
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
      ;; (if (not (null? includes))
      ;;     (begin
      ;;       (format #t "INCS: ~A\n" includes)
      ;;       (format #t "STRIPPED: ~A\n" stripped)))
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
  (if-let ((srcs (assoc-in '(:ocaml :static) (cadr srcfiles))))
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

(define (pkg-stanzas->public-names-table! pkg-path stanzas pname-tbl)
  ;; (format #t "pkg-stanzas->public-names-table! ~A\n ~A\n"
  ;;         pkg-path stanzas)
  (for-each
   (lambda (stanza)
     ;; stanza:: (<stanza-type> <alist>)
     ;; (format #t "STANZA: ~A\n" (car stanza))
     (let* ((stanza-alist (cadr stanza)))
       ;; stanza-alist: (<assoc> <assoc> ...)
       (case (car stanza)
         ((:library)
          (let* (;;(modname (assoc-in '(:name :module) stanza-alist))
                 (privname (assoc-in '(:name :private) stanza-alist))
                 (modname (normalize-module-name (cadr privname)))
                 (pubname (assoc-in  '(:name :public) stanza-alist))
                 (label (string-append
                         "//" pkg-path ":"
                         (if pubname
                             (symbol->string (cadr pubname))
                             (symbol->string (cadr privname)))))

                 (entry (if (and privname pubname)
                            `((:private ,(cadr privname))
                              (:public ,(cadr pubname))
                              (:module ,modname)
                              (:pkg ,pkg-path)
                              (:label ,label))
                            ;; else privname only
                            `((:private ,(cadr privname))
                              ;; set pubname <= privname?
                              (:pkg ,pkg-path)
                              (:label ,label)))))
            ;; (format #t "    modname: ~A\n" modname)
            ;; (format #t "    privname: ~A\n" privname)
            ;; (format #t "    pubname: ~A\n" pubname)

            ;; we always have a privname?
            (hash-table-set! pname-tbl (cadr privname) entry)
            (hash-table-set! pname-tbl modname entry)

            (if pubname
                (hash-table-set! pname-tbl (cadr pubname) entry))

            ;; (begin
            ;;   (hash-table-set! pname-tbl
            ;;                    (cadr pubname)
            ;;                    entry)
            ;;   ;; (cadr modname))
            ;;   (hash-table-set! pname-tbl
            ;;                    (cadr modname)
            ;;                    entry)
            ;;   ;; (cadr pubname))
            ;;   )
                ))
         ;; ((:executable)
         ;;  (begin
         ;;    (format #t "executable->pnt: ~A\n" stanza)
         ;;    ))
         ;; ((rule) ) ;; skip
       (else
        ;; skip
        ))))
     (cadr stanzas)))

;; initialize names lookup table. maps names to names and label, e.g.
;; one entry per name (private, public, module), values overlap
;; so we can easily find label for any of the three
;; used by e.g. lib 'select' LHS resolution.
;; FIXME: better naming
(define (libdeps->names-ht dune-pkg-tbls)
  (let ((pname-tbl (make-hash-table)))
    (for-each (lambda (pkg-tbl)
                ;;(format #t "PKG-TBL: ~A\n" (car pkg-tbl))
                ;; pkg-tbl: (<dir-path> <pkg hash-tbl>) i.e. assoc pair
                (for-each (lambda (pkg)
                            ;; pkg: (<pkg-path> <pkg-list>)
                            ;;(format #t "PKG: ~A\n" (car pkg))
                            (let ((stanzas (assoc :stanzas (cdr pkg))))
                              (if (cadr stanzas)
                                  (pkg-stanzas->public-names-table!
                                   (car pkg)
                                   stanzas pname-tbl)

                                  ;; else skip - pkg w/o dune stanzas
                                  )))
                          (cadr pkg-tbl)))
              dune-pkg-tbls)
    pname-tbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


