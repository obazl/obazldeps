(display "loading diagnostics.scm")

(load "dune.scm")

(define (print-multiple-lib-stanzas dune-pkg-tbls)
  (for-each
   (lambda (kv)
     ;; (display (format #f "KV: ~A" (assoc :stanzas (cdr kv)))) (newline)
     (let ((libs (assoc+ 'library
                         (cadr (assoc :stanzas (cdr kv))))))
       (if (> (length libs) 1)
           (begin
             (display (format #f "multilibs: ~A: ~A"
                              (car kv) (length libs)))
             (newline)))
       ))
   (car dune-pkg-tbls))
  '())

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
