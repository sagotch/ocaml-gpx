FLAGS     =-package str -package xml-light -package ISO8601
OCAMLFIND =ocamlfind
CAMLC     =$(OCAMLFIND) ocamlc $(FLAGS)
CAMLOPT   =$(OCAMLFIND) ocamlopt $(FLAGS)

LIBNAME   =gpx
LIBFILES  =gpx.cmi gpx.cma gpx.cmxa gpx.a

VERSION   =1.1.1

DOCDIR    =doc

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

doc:
	mkdir -p $(DOCDIR)
	ocamldoc -html -d $(DOCDIR) -I `ocamlfind query xml-light` gpx.mli

gh-pages:
	commitmsg="Documentation for $(VERSION) version." \
	docdir="$(DOCDIR)" \
	upstream="origin" \
	ghpup

clean:
	rm -rf *.cma *.cmxa *.cmi *.o *.cmo *.cmx *.a $(DOCDIR)
