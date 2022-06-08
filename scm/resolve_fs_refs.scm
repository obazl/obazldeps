;; dune glob syntax
;; https://dune.readthedocs.io/en/stable/concepts.html#glob
;; * anything before the last / is taken as a literal path
;; * anything after the last /, or everything if the glob contains
;;   no /, is interpreted using the glob syntax

;; \<char> matches exactly <char>, even if it is a special character
;; (*, ?, …)

;; * matches any sequence of characters, except if it comes first in
;; which case it matches any character that is not . followed by
;; anything

;; ** matches any character that is not . followed by anything, except
;; if it comes first in which case it matches anything

;; ? matches any single character

;; [<set>] matches any character that is part of <set>

;; [!<set>] matches any character that is not part of <set>

;; {<glob1>,<glob2>,...,<globn>} matches any string that is matched by
;; one of <glob1>, <glob2>, …

(define (dune-glob->files pkg-path dir glob)
  (format #t "dune-glob->files: ~A ~A\n" dir glob)
  (if-let ((idx (string-index glob (lambda (ch) (char=? ch #\*)))))
          (let ((pattern (string-append pkg-path "/" dir "/" glob)))
            (format #t "globbing: ~A\n" pattern)
            (fs-glob->list pattern))
          (string-append dir "/" glob)))

(define (resolve-files pkg-path args)
  (format #t "RESOLVE-FILES: ~A\n" args)
  (if (pair? (car args))
      (if (equal? (caar args) 'files)
          (begin
            (format #t "FILES ...\n")
            args)
          (error 'bad-arg
                 (format #f "expected 'files', got ~A\n" (caar args))))
      (begin
        (format #t "atom: ~A\n" (car args))
        (let* ((arg (car args))
               (str (if (symbol? arg) (symbol->string arg) arg))
               (dir (dirname str))
               (base (basename str)))
          (if base
              (let ((files (dune-glob->files pkg-path dir base)))
                files)
              dir)))))
