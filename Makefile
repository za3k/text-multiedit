LIBS=utils.ml color.ml output.ml input.ml types.ml constants.ml debug.ml text.ml args.ml common.ml display.ml client.ml server.ml

run: editor
	./$< --stand-alone TESTING.txt
test: test.exe
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
test.exe: $(LIBS) test.ml
	ocamlfind ocamlc -linkpkg -package ounit2 -o $@ $^
%.o: %.ml
	ocamlc -o $@ $<
