LIBS=unix
PROGRAM=kettle.native

all:
	ocamlbuild -libs $(LIBS) $(PROGRAM);
	@mv kettle.native kettle 

clean:
	ocamlbuild -clean

.PHONY: all clean
