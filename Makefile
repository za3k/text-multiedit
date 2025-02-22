LIBS=input.ml output.ml types.ml debug.ml text.ml editor.ml

run: editor
	./$<
test: test
	./$<; rm *.log *.cache
clean:
	rm -f *.cmi *.cmo *.o editor a.out *.cmx
cloc: $(LIBS) main.ml Makefile
	cloc $^ | head -n6 | tail -n4
# This warns about linking glibc statically
# Chronic suppresses the warning for me (provided in joeyhess's miscutils)
editor: $(LIBS) main.ml
	chronic ocamlopt -g -ccopt -static -I +unix unix.cmxa -o $@ $^
	rm -f *.cmi *.cmo *.cmx *.o
test: $(LIBS) test.ml
	ocamlfind ocamlc -linkpkg -package ounit2 -o $@ $^
%.o: %.ml
	ocamlc -o $@ $<
