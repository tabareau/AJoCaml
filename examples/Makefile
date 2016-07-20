all: build

SRC= $(wildcard *.aml)
OBJ= $(SRC:.aml=.ml)

build: $(OBJ)

%.ml: %.aml
	../ajocaml/ajocaml.native $< 

clean:
	rm -f $(OBJ)