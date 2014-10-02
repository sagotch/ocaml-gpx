FLAGS     =-package str -package xml-light
OCAMLFIND =ocamlfind
CAMLC     =$(OCAMLFIND) ocamlc $(FLAGS)
CAMLOPT   =$(OCAMLFIND) ocamlopt $(FLAGS)

LIBNAME   =gpx
LIBFILES  =gpx.cmi gpx.cma gpx.cmxa gpx.a

.POSIX:

all: build install

build:
	$(CAMLC) -c gpx.mli
	$(CAMLC) -a -o gpx.cma gpx.ml
	$(CAMLOPT) -a -o gpx.cmxa gpx.ml

install: $(LIBFILES)
	$(OCAMLFIND) install $(LIBNAME) META $^

uninstall:
	$(OCAMLFIND) remove $(LIBNAME)

clean:
	rm -f *.cma *.cmxa *.cmi *.o *.cmo *.cmx
