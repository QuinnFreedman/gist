default: tests

OCAMLC_FLAGS=-g -quiet
BUILD= \
        ocamlbuild \
                -r \
                -I src \
                -use-ocamlfind \
                -quiet

main: gist.byte

tests: tests.byte
	./tests

%.byte:
	$(BUILD) $@
	@mv $@ $*

.PHONY: tests default
