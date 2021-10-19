;; (format #t "loading @camlark//scm/dune_ingest.scm\n")

(load "alist.scm")
(load "dune.scm")
(load "dune_stanzas.scm")
(load "utils.scm")

;; data structures

  ;; (display pkgs-map)
  ;; (display (format #f "entry count: ~D"
  ;;                  (hash-table-entries pkgs-map))) (newline)
  ;; (newline)

  ;; (emit-codept-args)

  ;; "fileseq ok")

;; (define (-add-dunefile pkgs-tbl pfx dirent)
;;   (if-let ((pkg-alist (hash-table-ref pkgs-tbl pfx)))
;;           (begin
;;             (hash-table-set! pkgs-tbl pfx
;;                              (assoc-update :dunefile
;;                                            pkg-alist
;;                                            (lambda (old)
;;                                              (cons dirent
;;                                                    old))))
;;             pkgs-tbl)
;;           (begin
;;             (hash-table-set! pkgs-tbl pfx
;;                              (list
;;                               (list :dunefile (list dirent))))
;;             pkgs-tbl)))

(define (path->label path)
  path)

(define (-add-ocaml-srcfile pkgs-tbl pfx file)
  ;; (format #t "add-ocaml-srcfile: ~A/~A\n" pfx file) ;; pkgs-tbl))
  (let ((pkg-lbl (path->label pfx)))
    (if-let ((pkg-alist (hash-table-ref pkgs-tbl pkg-lbl)))
            ;; update existing pkg-alist
            (begin
              ;; (if (string=? pfx "src/bin_client")
              ;;     (format #t "PKG-ALIST ~A\n" pkg-alist))
              ;; (let ((upd (begin
              ;;              (alist-update-in!
              ;;               pkg-alist
              ;;               '(:srcfiles :ocaml)
              ;;               (lambda (old-assoc)
              ;;                 (if (null? old-assoc)
              ;;                     (begin
              ;;                       ;; (format #t "ADDNEW file: ~A\n"
              ;;                       ;;         file)
              ;;                       (list (list file)))
              ;;                     (begin
              ;;                       ;; (format #t "ADD file: ~A :: ~A\n"
              ;;                       ;;      file old-assoc)
              ;;                       (list (cons file (cadr old-assoc)
              ;;                                   ))))))
              ;;              pkg-alist)))
                ;; (if (string=? pfx "src/bin_client")
                ;;(format #t "UPD: ~A\n" upd)
                (hash-table-set! pkgs-tbl pkg-lbl ;; pkg-alist)
                                 (begin
                                   (alist-update-in!
                                    pkg-alist
                                    '(:srcfiles :ocaml)
                                    (lambda (old-assoc)
                                      (if (null? old-assoc)
                                          (begin
                                            ;; (format #t "ADDNEW file: ~A\n"
                                            ;;         file)
                                            (list (list file)))
                                          (begin
                                            ;; (format #t "ADD file: ~A :: ~A\n"
                                            ;;      file old-assoc)
                                            (list (cons file (cadr old-assoc)
                                                        ))))))
                                   pkg-alist))
                ;; )
            pkgs-tbl)
            ;; add new pkg-alist
            (begin
              ;; (format #t "FIRST file: ~A\n" file)
              (hash-table-set! pkgs-tbl pkg-lbl
                               (list
                                (list :srcfiles
                                      (list (list :ocaml (list file))))))
              pkgs-tbl))))

(define (-add-srcfile pkgs-tbl pfx file typ)
  ;; typ :: :sh | :mll | :mly
  ;; (format #t "add-sh-srcfile ~A:~A\n" pfx file) ;; pkgs-tbl))
  (let ((pkg-lbl (path->label pfx)))
    (if-let ((pkg-alist (hash-table-ref pkgs-tbl pkg-lbl)))
            ;; update existing pkg-alist
            (begin
              ;; (if (string=? pfx "src/lib_stdlib")
              ;;     (format #t "PKG-ALIST ~A\n" pkg-alist))
              (hash-table-set! pkgs-tbl pkg-lbl
                               (begin
                                 (alist-update-in!
                                  pkg-alist
                                  `(:srcfiles ,typ)
                                  (lambda (old-assoc)
                                    (if (null? old-assoc)
                                        (begin
                                          ;; (format #t "ADDNEW file: ~A\n"
                                          ;;         file)
                                          (list (list file)))
                                        (begin
                                          ;; (format #t "ADD file: ~A :: ~A\n"
                                          ;;      file old-assoc)
                                          (list (cons file (cadr old-assoc)
                                                )))))
                                  ;; (lambda (old-assoc)
                                  ;;   (if (null? old-assoc)
                                  ;;       file
                                  ;;       (cons file (cdr old-assoc)
                                  ;;             )))
                                  )
                                 pkg-alist)
                               )
              pkgs-tbl)
            ;; add new pkg-alist
            (begin
              (hash-table-set! pkgs-tbl pkg-lbl
                               (list
                                (list :srcfiles
                                      (list (list typ (list file))))))
              pkgs-tbl))))

(define (ocaml_srcfile? f)
  (or (string-suffix? ".mli" f)
      (string-suffix? ".ml" f)))

(define (-add-dune-project pkgs-tbl pfx dirent)
  ;; (display (format #f "dune-project in ~A" pfx)) (newline)
  ;; TODO: do we need to do anything with dune-project files?
  pkgs-tbl)

;; fold accumulation function
;; MUST RETURN PKGS-TBL
(define (fold-file pkgs-tbl pfx dirent)
  ;; (display (format #f "fold-file tbl: ~A, pfx: ~A, dirent: ~A"
  ;;                  pkgs-tbl pfx dirent))
  ;; (newline)
  ;; FIXME: bazel-* may be changed by --output_base
  (if (or (char=? #\. (string-ref dirent 0))
          (string-position "bazel-" dirent))
      ;; skip '.', '..', hidden files, bazel-* files
      pkgs-tbl
      (cond
       ((ocaml_srcfile? dirent)
        (let ((updated-pkgs-tbl (-add-ocaml-srcfile pkgs-tbl pfx dirent)))
          updated-pkgs-tbl))

       ((string-suffix? ".sh" dirent)
        (let ((updated-pkgs-tbl (-add-srcfile pkgs-tbl pfx dirent :sh)))
          updated-pkgs-tbl))

       ((string-suffix? ".mll" dirent)
        (let ((updated-pkgs-tbl (-add-srcfile pkgs-tbl pfx dirent :mll)))
          updated-pkgs-tbl))

       ((string-suffix? ".mly" dirent)
        (let ((updated-pkgs-tbl
               (-add-srcfile pkgs-tbl pfx dirent :mly)))
          updated-pkgs-tbl))

       ((equal? dirent "dune-project")
        (begin
          (-add-dune-project pkgs-tbl pfx dirent)))
       ;; skip other files
       (else pkgs-tbl))))

(define (normalize-dune-modules-field modules codept-pkg-tbl)
  ;; normal form:
  ;; (modules ((amod ((:name Amod)
  ;;                  (:ml amod.ml :deps (deps from  codept ...))
  ;;                  (:mli amod.mli :deps (deps from  codept ...))))
  ;;           (bmod ((:name Bmod)
  ;;                  (:ml bmod.ml :deps (deps from  codept ...))
  ;;                  (:mli bmod.mli :deps (deps from  codept ...))))))
  (map (lambda (module)
         module)
       modules)
  )

(define (dune-sexps->pkg pfx dune-stanzas)
  ;; (format #t "dune-sexps->pkg pfx ~A, sexps: ~A\n"
  ;;                  pfx dune-stanzas)

  ;; to normalize 'modules' fld we need list of src files, so we defer
  ;; stanza normalization until we've crawled the whole tree -
  ;; normalize-pkg-tbl in dune.scm

  (let ((stanzas dune-stanzas))
  ;;       (map
  ;;                 (lambda (stanza)
  ;;                   ;;(normalize-dune-stanza stanza))
  ;;                   stanza)
  ;;                 dune-stanzas)))
    (list
     (list :pkg-path pfx)
     (cons :stanzas (list ;; make it an alist
                     stanzas)))))

(define (dunefile->pkg-alist pfx); dirlist)
  ;; (format #t "dunefile->pkg-alist pfx: ~A\n" ;;, dirlist: ~A\n"
  ;;                  pfx) ;; dirlist)
  (if (not (directory? pfx))
      (error 'wrong-type-arg (string-append pfx ": not a directory")))
  (let ((dunefile-path (string-append pfx "/dune")))
    (if (file-exists? dunefile-path)
        (let* ((dune-sexps (read-dunefile pfx "dune")) ;; dunefile-path))
               (dune-pkg (dune-sexps->pkg pfx dune-sexps)))
          ;; (format #t "-> dune pkg: ~A\n" dune-pkg)
          dune-pkg)
        #f)))

;; recursively crawl the tree, ingesting dunefiles
;; dunefile in current dir (i.e. dirents list) already processed
;; i.e. process dunefiles when we find subdirs: first process
;; subdir/dune, then recur on subdir
;; at most one pkg-tbl per directory
(define (-dir-fold fn pkg-tbl pfx dirents)
  ;; (display (format #f "-dir-fold: ~A" pfx))
  ;;                  ;; pkg-tbl pfx dirents))
  ;; (newline)
  (if (null? dirents)
      pkg-tbl
      (if (or (equal? ".." (car dirents)) (equal? "." (car dirents)))
          ;; skip '.' and  '..'
          (-dir-fold fn pkg-tbl pfx (cdr dirents))
          ;; process first dirent
          (let* ((hd-file (car dirents))
                 (subdir-pfx (string-append pfx "/" hd-file)))
            ;; (format #t "FOLDING ~A\n" subdir-pfx)
            (if (directory? subdir-pfx)
                ;; subdir: first process its dunefile, then recur on it
                (let* ((subdir-dirents (directory->list subdir-pfx))
                       (dune-pkg (dunefile->pkg-alist subdir-pfx)))
                                                      ;; subdir-dirents)))
                  (if dune-pkg
                      (if (hash-table-ref pkg-tbl
                                          (path->label subdir-pfx))
                          (error 'dup-pkg-path
                                 (string-append "duplicate pkg path: "
                                                subdir-pfx))

                          ;; else new: put dune pkg in pkg-tbl, then process
                          ;; entire subdir
                          (begin
                            ;; (format #t ">>dune-pkg: ~A\n" dune-pkg)
                            ;; add subdir dune pkg to pkgs table
                            (hash-table-set! pkg-tbl
                                             (cadr (assoc :pkg-path
                                                          dune-pkg))
                                             ;; (path->label subdir-pfx)
                                             dune-pkg)
                            ;; now process dirents of subdir
                            (let ((pt (-dir-fold fn
                                                pkg-tbl
                                             (cadr (assoc :pkg-path
                                                          dune-pkg))
                                                ;; subdir-pfx
                                                subdir-dirents)))
                              ;; and process remainder of dirents in cwd
                              (-dir-fold fn
                                         pt
                                         pfx (cdr dirents)))))
                      ;; no dunefile found in subdir
                      ;; process subdir
                      (let ((pt (-dir-fold fn
                                           pkg-tbl
                                           subdir-pfx subdir-dirents)))
                        ;; and process remainder of dirents in cwd
                        (-dir-fold fn
                                   pt
                                   pfx (cdr dirents)))))
                  ;; subdir does not contain dunefile
                ;; (-dir-fold fn (fn pkg-tbl pfx hd-file) pfx (cdr dirents)))
                ;; subdir-pfx is a file, not a subdirectory
                (-dir-fold fn (fn pkg-tbl pfx hd-file) pfx (cdr dirents)))))))

;; src tree -> pkgs table
;; fixme:  add :include, :exclude args
(define (dune-fold-dirs dirs)
  ;; (display (format #f "dune-fold-dirs: ~A" dirs)) (newline)
  (map (lambda (dir)
         ;; (display (format #f "dune-fold-dirs: ~A" dir)) (newline)
         (if (char=? #\/ (string-ref dir 0))
             (error 'wrong-type-arg "paths must be relative"))
         (if (not (directory? dir))
             (error 'wrong-type-arg
                    (string-append "Not a directory: " dir)))
         (let ((pkg-tbl
                (-dir-fold fold-file (make-hash-table) dir (directory->list dir))))

           (list dir (normalize-pkg-tbl pkg-tbl))
           ;;pkg-tbl
           ))
         ;; (fold-dirtree fold->srcfiles dir))
       dirs))
