clean:
	ocamlbuild -clean

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

compile:
	ocamlbuild -use-ocamlfind player.cmo
	ocamlbuild -use-ocamlfind state.cmo
	ocamlbuild -use-ocamlfind command.cmo
play:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte
