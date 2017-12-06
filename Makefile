test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

tex:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

pdf:
	make tex && open output.pdf

ast:
	ocamlbuild -use-ocamlfind ast.byte

parse:
	ocamlbuild -use-ocamlfind parse.byte && ./parse.byte

clean:
		ocamlbuild -clean
		rm *.aux *.log *.tex *.pdf
