test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

pdf:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte && open output.pdf

ast:
	ocamlbuild -use-ocamlfind ast.byte

parse:
	ocamlbuild -use-ocamlfind parse.byte && ./parse.byte

clean:
		ocamlbuild -clean
