
;; ocaml/dune
;; (rule
;;  (copy main.exe ocamlc.byte))

;; (rule
;;  (copy optmain.exe ocamlopt.byte))

;; ocaml/lambda/dune:

(define ocaml/lambda-path "lambda")
(define ocaml/lambda-srcs (directory->list ocaml/lambda-path))

(define pkg-path "lambda")
