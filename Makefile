CMO= lexer.cmo parser.cmo x86_64.cmo typing.cmo bcheck.cmo precomp.cmo compile.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli 
BIN=prustc
FLAGS=
CC=ocamlc

all: $(BIN)

tests: $(BIN)
	


$(BIN): $(CMO)
	$(CC) $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	$(CC) $(FLAGS) -c  $<

.ml.cmo:
	$(CC) $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir --infer -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.automaton

parser.ml: ast.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



