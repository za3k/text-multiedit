run: editor.o
	./$<
test: test.o
	./$<
editor.o: text.ml editor.ml
	ocamlc -I +unix unix.cma -o $@ $^
test.o: text.ml test.ml
	ocamlfind ocamlc -linkpkg -package ounit2 -o $@ $^
%.o: %.ml
	ocamlc -o $@ $<
