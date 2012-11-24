ocamlopt -c dimacs_cnf.ml

ocamlopt -c skyscraper_encoder.ml
ocamlopt dimacs_cnf.cmx skyscraper_encoder.cmx -o skyscraper_encoder

ocamlopt -c skyscraper_decoder.ml
ocamlopt dimacs_cnf.cmx skyscraper_decoder.cmx -o skyscraper_decoder
