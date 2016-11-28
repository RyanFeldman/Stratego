default:
	ocamlbuild board.byte
	ocamlbuild ai.byte
	ocamlbuild display.byte
	ocamlbuild game.byte
	ocamlbuild main.byte

test:
	ocamlbuild -pkgs oUnit,str,unix test.byte && ./test.byte

play:
	ocamlbuild -pkgs oUnit,ANSITerminal main.byte && ./main.byte