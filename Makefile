ast:
	ocamlbuild -use-ocamlfind ast.byte

parse:
	ocamlbuild -use-ocamlfind parse.byte && ./parse.byte

main:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte
