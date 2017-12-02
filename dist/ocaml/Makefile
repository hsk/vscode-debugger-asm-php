all:
	ocamlyacc parser.mly
	rm -rf parser.mli
	ocamllex lexer.mll
	ocamlfind ocamlc -g -o ../server -package yojson -linkpkg ast.ml parser.ml lexer.ml vM.ml debugSession.ml asmDebugSession.ml main.ml
	rm -rf *.cm* parser.ml lexer.ml
	cd ../../; make
opam:
	opam install yojson
clean:
	rm -rf *.cm* parser.ml lexer.ml server

