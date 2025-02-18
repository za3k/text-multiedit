LIBS=types.ml text.ml debug.ml editor.ml

run: editor
	./$<
test: test.o
	./$<; rm *.log *.cache
clean:
	rm -f *.cmi *.cmo *.o editor a.out *.cmx
cloc: $(LIBS) main.ml
	cloc $^ | head -n6 | tail -n4
# This warns about linking glibc statically -- switch to musl if wanted, or suppress the warning
editor: $(LIBS) main.ml
	ocamlopt -ccopt -static -I +unix unix.cmxa -o $@ $^ #2>&1 | grep -vE "requires at runtime|unix.n.o|libunixnat.a" || true
test.o: $(LIBS) test.ml
	ocamlfind ocamlc -linkpkg -package ounit2 -o $@ $^
%.o: %.ml
	ocamlc -o $@ $<
