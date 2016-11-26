default:
	ocamlbuild board.byte
	ocamlbuild ai.byte

test:
	ocamlbuild -pkgs oUnit,str,unix test.byte && ./test.byte

play:
	ocamlbuild -pkgs oUnit,ANSITerminal main.byte && ./main.byte