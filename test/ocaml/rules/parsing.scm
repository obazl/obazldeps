;; ocaml/parsing/dune


(ocamllex
 (modules lexer)
 (mode fallback))

;; ocaml/ocamltest/dune

(ocamllex
 (modules tsl_lexer)
 (mode fallback))

(ocamlyacc
 (modules tsl_parser)
 (mode fallback))

