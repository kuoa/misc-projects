OC = ocamlc
LIB = str.cma

SRC = ca.ml
SRCO = $(SRC:.ml=.cmo)
SRCI = $(SRC:.ml=.cmi)

MAIN = p47.ml
MAINO = $(MAIN:.ml=.cmo)
MAINI = $(MAIN:.ml=.cmi)

EXEC = run

all: $(EXEC)

$(EXEC): $(SRCO) $(MAINO) $(SRCI)
	$(OC) -o $@ $(LIB) $(SRCO) $(MAINO)

$(MAINO): $(MAIN)
	$(OC) -c $(LIB) $<

%.cmo: %.ml %.cmi
	$(OC) -c $(LIB) $<

%.cmi: %.mli
	$(OC) -c $(LIB) $<




clean:
	rm -f $(SRCO) $(SRCI) $(MAINO) $(MAINI) $(EXEC) 
