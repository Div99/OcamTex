test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

main:
	ocamlbuild -use-ocamlfind main.byte

tex:
	make main && ./main.byte test.otex

clean:
		ocamlbuild -clean
		rm *.aux *.log *.tex *.pdf
