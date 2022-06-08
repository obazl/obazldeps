(define proj-root (string-append (getenv "HOME") "/ocaml/ocaml/"))

;; ocaml/toplevel/dune

;; %{ocaml_where} == output of `ocamlc -where`
;; %{ocaml_where}/expunge is a byte-compiled executable

(define ocaml.byte
  '((:pkg-path "toplevel")
    (:rule (rule
            (targets ocaml.byte)
            (action (run %{ocaml_where}/expunge %{dep:topstart.exe} %{targets}
                                        ; FIXME: inlined $(STDLIB_MODULES) ... minus Labels ones ...
                         stdlib__Arg
                         stdlib__Array
                                        ; stdlib__ArrayLabels
                         stdlib__Bigarray
                         stdlib__Buffer
                         stdlib__Bytes
                                        ; stdlib__BytesLabels
                         stdlib__Callback
                         camlinternalFormat
                         camlinternalFormatBasics
                         camlinternalLazy
                         camlinternalMod
                         camlinternalOO
                         stdlib__Char
                         stdlib__Complex
                         stdlib__Digest
                         stdlib__Either
                         stdlib__Ephemeron
                         stdlib__Filename
                         stdlib__Float
                         stdlib__Format
                         stdlib__Gc
                         stdlib__Genlex
                         stdlib__Hashtbl
                         stdlib__Int32
                         stdlib__Int64
                         stdlib__Lazy
                         stdlib__Lexing
                         stdlib__List
                                        ; stdlib__ListLabels
                         stdlib__Map
                         stdlib__Marshal
                                        ; stdlib__MoreLabels
                         stdlib__Nativeint
                         stdlib__Obj
                         stdlib__Oo
                         stdlib__Option
                         stdlib__Parsing
                         stdlib__Pervasives
                         stdlib__Printexc
                         stdlib__Printf
                         stdlib__Queue
                         stdlib__Random
                         stdlib__Result
                         stdlib__Scanf
                         stdlib__Seq
                         stdlib__Set
                         stdlib__Stack
                                        ; stdlib__StdLabels
                         stdlib
                         stdlib__Stream
                         stdlib__String
                                        ; stdlib__StringLabels
                         stdlib__Sys
                         stdlib__Uchar
                         stdlib__Weak
                                        ; the rest
                         outcometree topdirs topeval toploop topmain topcommon
                         ))))))
