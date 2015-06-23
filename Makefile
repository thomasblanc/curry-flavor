build:
	ocaml setup.ml -build

configure:
	ocaml setup.ml -configure

install:
	ocaml setup.ml -install

test:
	ocaml setup.ml -test

all: build test

clean:
	ocaml setup.ml -clean

.PHONY: configure all clean build test install
