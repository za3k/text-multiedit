run: text.o
	./$<
text.o: text.ml
	ocamlc -I +unix unix.cma -o $@ $<
%.o: %.ml
	ocamlc -o $@ $<
