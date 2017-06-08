OCAMLBUILDFLAGS = -I src -I tests -use-ocamlfind -pkgs oUnit,batteries,zarith
ifeq ($(DEBUG), 1)
	OCAMLBUILDFLAGS += -tag debug -ocamlopt "ocamlopt -p"
endif
OCAMLBUILD = ocamlbuild $(OCAMLBUILDFLAGS)

MLFILES = src/*.ml src/*.mli tests/*.ml tests/*.mli

TESTARGS = -shards 4

default: test.native ntruparams.native ntrudemo.native doc

test: test.native
	./test.native $(TESTARGS)

draft: paper/draft.pdf

paper/draft.pdf: paper/*.tex paper/*.bib
	latexmk -pdf -cd paper/draft.tex

%.native: $(MLFILES)
	$(OCAMLBUILD) $@

clean:
	rm -rf _build
	$(OCAMLBUILD) -clean

# for some reason running it several times helps it figure out where submodules
# are?????
doc:
	$(OCAMLBUILD) ntru.docdir/index.html
	$(OCAMLBUILD) ntru.docdir/index.html
	$(OCAMLBUILD) ntru.docdir/index.html

.PHONY: default test paper draft clean doc
