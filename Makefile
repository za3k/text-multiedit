LIBS=types.ml debug.ml text.ml editor.ml

run: editor
	./$<
test: test.o
	./$<; rm *.log *.cache
clean:
	rm -f *.cmi *.cmo *.o editor a.out *.cmx
cloc: $(LIBS) main.ml
	cloc $^ | head -n6 | tail -n4
# This warns about linking glibc statically
# Chronic suppresses the warning for me (provided in joeyhess's miscutils)
editor: $(LIBS) main.ml
	chronic ocamlopt -ccopt -static -I +unix unix.cmxa -o $@ $^
test.o: $(LIBS) test.ml
	ocamlfind ocamlc -linkpkg -package ounit2 -o $@ $^
%.o: %.ml
	ocamlc -o $@ $<
