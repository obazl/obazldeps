;; camlark/test contains scm files containing rule stanzas from
;; various projects; for now just tezos and the ocaml compiler.

;; To test from the repl, we want to run `normalize-stanza-rule`,
;; passing the pkg-path (relative to proj root), the list of srcfiles
;; from the pkg-path dir, and the stanza (obtained from the
;; camlark/test subdir).

;; So we need to run scheme in the project root so that we can get

;; pkg-path may be used to check file existence

;; , but add the
;; camlark/test subdir to the load-path so we can get the stanza and
;; the pkg-path. Then we run `directory->list` on the pkg-path to get
;; srcfiles.


(begin
  (load "alist.scm")
  (load "dune.scm")
  (load "dune_stanzas.scm")
  (load "dune_stanza_rule.scm")
  (load "utils.scm"))

(define camlark-root (string-trim '(#\space #\newline) (system "pwd" #t)))

;; (begin
;;   (chdir camlark-root)
;;   (load "test/tezos/with-stdout-to.scm")
;;   (define stanza-pkg sigs/v0)
;;   )

;; (begin
;;   (chdir camlark-root)
;;   (load "test/tezos/bash.scm")
;;   (define stanza-pkg alias/runtest_rejections)
;;   )

(begin
  (chdir camlark-root)
  (load "test/tezos/bash.scm")
  (define stanza-pkg alias/runtest_rejections)
  )

;; (begin
;;   (chdir camlark-root)
;;   (load "test/ocaml/rules/with-stdout-to.scm")
;;   ;;(define stanza-pkg ocaml/lambda)
;;   ;;(define stanza-pkg ocaml/parsing)
;;   ;;(define stanza-pkg ocaml/runtime/caml)
;;   (define stanza-pkg ocaml/utils)
;;   )

;; (begin
;;   (chdir camlark-root)
;;   (load "test/ocaml/rules/bash.scm")
;;   (define stanza-pkg ocaml/utils)
;;   )

;; (begin
;;   (chdir camlark-root)
;;   (load "test/ocaml/rules/write-file.scm")
;;   ;;(define stanza-pkg empty)
;;   (define stanza-pkg runtime)
;;   )

;; (begin
;;   (chdir camlark-root)
;;   (load "test/ocaml/rules/run.scm")
;;   (define stanza-pkg ocaml.byte)
;;   )

;; (begin
;;   (chdir camlark-root)
;;   (load "test/ocaml/rules/progn.scm")
;;   (define stanza-pkg libocamlrun)
;;   )

;; (begin
;;   (chdir camlark-root)
;;   (load "test/ocaml/rules/copy.scm")
;;   ;;(define stanza-pkg optmain.exe)
;;   ;;(define stanza-pkg lambda/main.exe)
;;   ;;(define stanza-pkg lambda/camlinternalMenhirLib.ml)
;;   ;;(define stanza-pkg lambda/camlinternalMenhirLib.mli)
;;   (define stanza-pkg lambda/libruntime_stubs.a)
;; )

;; normalize-stanza-rule
(define nx
  (let* (; (pkg rule)
         (pkg-path (cadr (assoc :pkg-path stanza-pkg)))
         (src-path (string-append proj-root pkg-path))
         (srcfiles (directory->list src-path))
         (rule (cadr (assoc :rule stanza-pkg))))
    (format #t "pkg-path: ~S\n" pkg-path)
    (format #t "srcfiles: ~A\n" srcfiles)
    (format #t "rule: ~A\n" rule)
    (load "dune_stanza_rule.scm")
    (load "dune_actions.scm")
    (chdir proj-root)
    (normalize-stanza-rule pkg-path
                           srcfiles
                           rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin
  (chdir camlark-root)
  (load "test/ocaml/stanzas/copy_stanzas.scm")
  (define stanza-pkg toplevel/byte)
  ;;(define stanza-pkg toplevel/byte2)
)

;; normalize-dune-stanza
(define nx
  (let* (; (pkg rule)
         (pkg-path (cadr (assoc :pkg-path stanza-pkg)))
         (src-path (string-append proj-root pkg-path))
         (srcfiles (directory->list src-path))
         (stanza (cadr (assoc :stanza stanza-pkg))))
    (format #t "pkg-path: ~S\n" pkg-path)
    (format #t "srcfiles: ~A\n" srcfiles)
    (format #t "stanza: ~A\n" stanza)
    (load "dune_stanzas.scm")
    (chdir proj-root)
    (normalize-dune-stanza pkg-path
                           ;; dune-project-stanzas
                           srcfiles
                           stanza)))

nx
(cdr nx)
(assoc :raw (cdr nx))
(assoc :cmd (cdr nx))
(assoc-in '(:cmd :tool) (cdr nx))
(assoc-in '(:cmd :args) (cdr nx))
(assoc-in '(:cmd :deps) (cdr nx))

