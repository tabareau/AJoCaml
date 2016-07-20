all: aj ex build

AJOCAML = ajocaml
EXAMPLES = examples
LIB = decentralized_weaving

aj:
	$(MAKE) -C $(AJOCAML)

ex:
	$(MAKE) -C $(EXAMPLES)

FILES = $(EXAMPLES)/aCache.ml $(EXAMPLES)/main.ml $(EXAMPLES)/aspect_repl.ml $(EXAMPLES)/client.ml $(EXAMPLES)/test_causedby_base.ml $(EXAMPLES)/test_causedby_aspect.ml 

build: 
	ocamlbuild -use-jocaml -I $(LIB) $(FILES:.ml=.byte)
# native)

clean:	
	$(MAKE) -C $(AJOCAML) clean
	$(MAKE) -C $(EXAMPLES) clean
	ocamlbuild -use-jocaml -clean -I $(LIB) $(FILES:.ml=.byte) 
#native)
