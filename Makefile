run: editor
	./$<
test: test.o
	./$<; rm *.log *.cache
clean:
	rm -f *.cmi *.cmo *.o editor a.out *.cmx
cloc: text.ml editor.ml
	cloc $^ | head -n6 | tail -n4
# This warns about linking glibc statically -- switch to musl if wanted, or suppress the warning
editor: text.ml editor.ml
	ocamlopt -ccopt -static -I +unix unix.cmxa -o $@ $^
test.o: text.ml test.ml
	ocamlfind ocamlc -linkpkg -package ounit2 -o $@ $^
%.o: %.ml
	ocamlc -o $@ $<
