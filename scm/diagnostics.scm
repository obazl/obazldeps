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
