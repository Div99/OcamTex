test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

main:
	ocamlbuild -use-ocamlfind main.byte

tex:
	make main && ./main.byte test.otex

pdf:
	make tex && open test.pdf

ast:
	ocamlbuild -use-ocamlfind ast.byte

parse:
	ocamlbuild -use-ocamlfind parse.byte && ./parse.byte

clean:
		ocamlbuild -clean
		rm *.aux *.log *.tex *.pdf
