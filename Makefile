test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

main:
	ocamlbuild -use-ocamlfind main.byte 

tex:
	make main && ./main.byte

parse:
	ocamlbuild -use-ocamlfind parse.byte && ./parse.byte

clean:
		ocamlbuild -clean
		rm *.aux *.log *.tex *.pdf
