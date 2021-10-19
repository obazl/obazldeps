(define (module->opam-label module)
  (string-append "@" module "//:" module))

;; returns string
(define (maybe-opam module)
  ;; (format #t "maybe-opam: ~A\n" module)
  (let* ((module-str (if (symbol? module)
                         (symbol->string module)
                         module)))

    (string-set! module-str 0 (char-downcase (string-ref module-str 0)))

    (let ((ocamlfind-pkg (system
                          (string-append "ocamlfind query " module-str
                                         " 2> /dev/null")
                          )))
      ;; (format #t "ocamlfind-pkg: ~A\n" ocamlfind-pkg)
      (if (equal? ocamlfind-pkg 0)
      ;; (if (char=? (string-ref ocamlfind-pkg 0) #\/)
          module-str
          #f))))
          ;; (let ((module-str (endash module-str)))
          ;;   (if (file-exists? (string-append "/Users/gar/.opam/4.10/lib/"
          ;;                                    module-str))
          ;;       module-str
          ;;       #f))))))

(define (update-opam-table module) ;; opam-tbl module)
  ;; (format #t "update-opam-table: ~A\n" module)
  (let* ((module module)
         (resolved (modules-tbl module)))
    (if (not resolved)
        (if-let ((opam (maybe-opam module)))
                (begin
                  (format #t "OPAM: ~A: ~A\n" module opam)
                  (hash-table-set! opam-tbl
                                   (string->symbol opam)
                                   (module->opam-label opam))
                  (hash-table-set! opam-tbl
                                   module
                                   (module->opam-label opam)))
                #f))))

(define (resolve-opam module)
  ;; (format #t "resolve-opam: ~A\n" module)
  (if-let ((opam (opam-tbl module)))
          opam
          (if-let ((opam (opam-tbl (normalize-module-name module))))
                  opam
                  (update-opam-table module))))

(define (codept->opam-table codept-sexp)
  (let* ((unresolved-modules (codept->unknown-modules codept-sexp))
         (opam-tbl (make-hash-table 1024 equal?)))
    (for-each (lambda (unresolved-module)
                ;; (format #t "unresolved-module: ~A\n" unresolved-module)
                ;; (format #t "  module: ~A\n" (caadar unresolved-module))
                ;; unresolved-module :: ((module (<name>)) (ml ...) (mli ...))

                (update-opam-table opam-tbl (car unresolved-module)))
         unresolved-modules)
    opam-tbl))

