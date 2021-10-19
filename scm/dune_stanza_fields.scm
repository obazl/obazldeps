(load "string.scm")
(load "srfi.scm")
(load "utils.scm")
(require write.scm)

;;fld: (name tezos_sapling)
;; FIXME: remove
(define (normalize-stanza-fld-name fld) ;; DEPRECATED
  ;; (format #t "normalize-stanza-fld-name ~A\n" fld)
  `(:name ((:private ,fld)
           (:module ,(normalize-module-name fld)))))

(define (normalize-stanza-fld-names fld)
  ;; (format #t "normalize-stanza-fld-names ~A\n" fld)
  (if (> (length fld) 1)
      (list :names fld)
      `(:name ((:private ,(car fld))
               (:module ,(normalize-module-name (car fld)))))))
 ;; (list :name (cons (car fld) (normalize-module-name (car fld))))))

;;fld: (public_name tezos-sapling)
(define (normalize-stanza-fld-public_name fld)
  ;; (format #t "normalize-stanza-fld-public_name ~A\n" fld)
  (list :public_name fld))

;;fld: (libraries ctypes ctypes.foreign ctypes.stubs hex data-encoding tezos-crypto tezos-stdlib tezos-error-monad tezos-lwt-result-stdlib)
;; tezos/src/lib_signer_backends/unix:
;; (libraries ocplib-endian.bigstring
;;            ... etc. ...
;;            (select ledger.ml from
;;              (ledgerwallet-tezos -> ledger.available.ml)
;;              (-> ledger.none.ml)))
;;  [why is a .ml file in a 'libraries' field?
(define (analyze-select deps directs selects)
  ;; FIXME: extract module dep from select sexp and add to directs
  directs)

(define (analyze-libdeps libdeps)
  ;; (format #t "analyze-libdeps: ~A\n" libdeps)
  (let recur ((deps libdeps)
              (directs '()) ;; directs == public_name
              (selects '())
              (modules '()))
    (if (null? deps)
        (values directs selects modules)
        (if (pair? (car deps))
            ;; e.g. (select ...)
            (if (equal? (caar deps) 'select)
                (recur (cdr deps)
                       (analyze-select deps directs selects)
                       (cons (car deps) selects)
                       (libdep->module-name modules))
                (error 'bad-lib-dep "embedded pair whose car is not 'select'"))
            (if (equal? 'libraries (car deps))
                ;; skip the initial fld name "libraries"
                (recur (cdr deps)
                       directs selects
                       (libdep->module-name modules))
                (recur (cdr deps)
                       (cons (car deps) directs)
                       selects
                       (libdep->module-name modules)))))))

(define (normalize-stanza-fld-libraries fld)
  ;; (format #t "normalize-stanza-fld-libraries: ~A\n" fld)
  (let-values (((directs selects modules) (analyze-libdeps fld)))
    (list :deps
          (list (list :raw fld)
                (list :selects (reverse selects))
                ;;(list :modules (reverse modules))
                (list :libs (reverse directs))))))

;; (modules Registerer), (modules (:standard \ legacy_store_builder))
;; (modules)
;; (modules (:standard (symbol "\\") delegate_commands delegate_commands_registration))
(define (normalize-stanza-fld-modules fld)
  (format #t "normalize-stanza-fld-modules: ~A\n" fld)
  ;; (cons 'modules
  ;;       (if (list? (cadr fld))
  ;;           (cdr fld)
  ;;           (list (cdr fld)))))
  fld)

;; e.g. (cdr (:standard (symbol "\\") legacy_store_builder))
(define (srcs->module-names srcfiles seq)
  ;; (format #t "srcs->module-names: ~A => ~A\n" srcfiles seq)
  (if (null? srcfiles)
      seq
      (let ((m (file-name->module-name (car srcfiles))))
        (if (member m seq)
            (srcs->module-names (cdr srcfiles) seq)
            (srcs->module-names (cdr srcfiles)
                                (cons m seq))))))

(define (normalize-stanza-lib-fld-modules srcfiles modules-assoc)
  ;; (format #t "normalize-stanza-lib-fld-modules: ~A\n" modules-assoc)
  ;; modules-assoc:: (modules <list of lists>)

  ;; modules list: each module is in a list (from codept sexp) whose
  ;; car may be a namespace module.
  (let* ((modules (cdr modules-assoc)))
    (if (null? modules)
        (values '() '())
        ;; (let ((result
               (let recur ((modules modules)
                           (direct '())
                           (indirect '()))
                 ;; (format #t "ms: ~A; direct: ~A\n" modules direct)
                 (cond
                  ((null? modules)
                   (values direct indirect))

                  ((equal? :standard (car modules))
                   (let ((newseq (srcs->module-names srcfiles direct)))
                     ;; (format #t "modules :STANDARD ~A\n" newseq)
                     ;; (format #t "CDRMODS ~A\n" (cdr modules))
                     (recur (cdr modules) (append newseq direct) indirect)))
                  ;; (concatenate direct
                  ;;              (norm-std-modules (cdr modules))))
                  ((pair? (car modules))
                   (let-values (((exp gen)
                                (recur (car modules) '() '())))
                     (recur (cdr modules)
                            (concatenate exp direct)
                            (concatenate gen indirect))))

                  ((indirect-module-dep? (car modules) srcfiles)
                   (begin
                     ;; (format #t "INDIRECT: ~A\n" (car modules))
                     (recur (cdr modules)
                            direct (cons (car modules) indirect))))

                  (else
                   (recur (cdr modules)
                          (cons (car modules) direct)
                          indirect))))
          ;;      ))
          ;; ;;(format #t "RESULT: ~A\n" result)
          ;; (reverse result))
               )))
     ;;      (norm-std-modules modules)
     ;;      (car modules)))
     ;;  (cons :modules (list (car modules)))
     ;; (else (cons :modules modules)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fields:  foreign_stubs (src files); foreign_archives (precompiled imports)
;; stanza type: foreign_library - dune builds a c archive
;; eg: (foreign_stubs (language c) (flags (-I%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs)) (names rustzcash_ctypes_c_stubs))
(define (normalize-stanza-fld-foreign_stubs fld)
  ;; (foreign_stubs...) is for foreign _sources_, compare foreign_archives
  ;; (display (format #f "FLD foreign_stubs: ~A" fld)) (newline)

  ;; subfields:
  ;; language
  ;; names - specifies the names of source files.
  ;; flags - passed when compiling source files.
  ;;  e.g. (flags (-I%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs))
  ;; include_dirs tracked as deps, passed to compiler via -I
  ;; extra_deps
  fld)

;;fld: (c_library_flags (-L%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs -lrustzcash -lpthread))
(define (normalize-stanza-fld-c_library_flags fld)
  fld)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  FLAGS
;; examples:
;; (flags (:standard -open Tezos_stdlib -open Tezos_crypto -open Tezos_error_monad -open Tezos_error_monad.TzLwtreslib))
;; (flags (-I%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs))
;; (flags -short-paths -g -w @a-4-29-40-41-42-44-45-48-58-59-60))
;; (flags :standard -short-paths -warn-error -58)
;; (flags -warn-error -22+34+33+27)
;; (flags (:standard -short-paths -cclib -ljemalloc -w @a-4-29-40-41-42-44-45-48-58-59-60-66))
;; (flags :standard -lstdc++ -lpthread))
;; (flags --explain --unused-tokens)
;; (flags (:include ../../discover/dune.flags))
;; (js_of_ocaml (flags +nat.js --source-map --pretty))

;; normal form:
;; (flags ((:standard -linkall -opaque ...)
;;         (:open '(Tezos_stdlib Tezos_crypto ...)))
;;         ...???)

;; (:standard -open Tezos_stdlib -open Tezos_crypto -open
;; Data_encoding -open Tezos_error_monad -open
;; Tezos_error_monad.TzLwtreslib -open Tezos_rpc -open Tezos_clic
;; -open Tezos_micheline -open Tezos_event_logging)

(define (normalize-open opener)
  ;; truncate at '__', e.g. Tezos_base__TzPervasives => Tezos_base
  ;; why? whatever the submod depends on will be listed in its aggregator

  (let ((ostr (symbol->string opener)))
    (if-let ((trunc-at (string-contains ostr "__")))
            (let ((result (string-take ostr trunc-at)))
              ;; (format #t "NORMED ~A => ~A\n" ostr result)
              result)
            ostr)))

(define (take-opens lst opens)
  ;; (format #t "take-opens")
  (if (null? lst)
      opens
      (let ((elt (car lst)))
        (if (symbol? elt)
            (if (equal? elt '-open)
                (take-opens (cddr lst)
                            (cons (normalize-open (cadr lst)) opens))
                (take-opens (cdr lst) opens))
            (if (string? elt)
                (if (string=? elt "-open")
                    (take-opens (cddr lst)
                                (cons (normalize-open (cadr lst)) opens))
                    (take-opens (cdr lst) opens))
                (take-opens (cdr lst) opens))))))

;; (flags :standard)
;; (flags (:standard -open Tezos_base__TzPervasives -open Tezos_micheline))
;; (:standard -linkall)
(define (normalize-stanza-fld-flags flags)
  ;; (display (format #f "FLD flags: ~A" flags)) (newline)
  (if flags
      (let ((flags (if (list? (cadr flags))
                       (cadr flags)
                       (list (cdr flags)))))
        ;; FIXME: expand :standard
        (let ((std (any (lambda (flag) (equal? flag :standard)) flags))
              (opens (take-opens flags '()))
              ;; (first-open (find-tail (lambda (elt)
              ;;                          (if (symbol? elt)
              ;;                              (equal? elt '-open)
              ;;                              (if (string? elt)
              ;;                                  (string=? "-open" elt)
              ;;                                  #f)))
              ;;                        flags))
              )
          ;; (display (format #f "opens: ~A" opens)) (newline)
          (list :opts
                (concatenate
                 (if std '((:standard)) '())
                 (if (null? opens) '() `((:opens ,opens)))
                 `((:raw ,flags))
                 `((:flags ,@(if std
                             (remove :item :standard flags)
                             flags)))))))
      #f))
