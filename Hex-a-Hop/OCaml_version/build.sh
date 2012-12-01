ocamlopt -c dimacs_cnf.ml
ocamlopt -c common.ml

ocamlopt -c encoder.ml
ocamlopt dimacs_cnf.cmx common.cmx encoder.cmx -o encoder

ocamlopt -c decoder.ml
ocamlopt dimacs_cnf.cmx common.cmx decoder.cmx -o decoder
