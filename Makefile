
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
BASEPACKAGES=bos,uutf,fmt,camlp5.extprint,pcre,yaml
PACKAGES=$(BASEPACKAGES),camlp5.extprint,camlp5.extend,camlp5.pprintf,pa_ppx.deriving_plugins.std,pa_ppx.base.link,pa_ppx.runtime,pa_ppx.testutils,sedlex

OBJ=jqtypes.cmo lazy_reclist.cmo jqutil.cmo jqlexing.cmo jqparse0.cmo jqinterp.cmo jqexec.cmo jq_examples.cmo
OML=jqtypes.ml lazy_reclist.ml jqutil.ml jqinterp.ml jqexec.ml jq_examples.ml jqtest.ml
LEXML=jqlexing.ml
RML=jqparse0.ml

all: $(OBJ) jqtest

test:: all
	mkdir -p _build
	./jqtest

jqtest: $(OBJ) jqtest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

jqparse0.cmo: jqparse0.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.SUFFIXES: .mll .ml .cmo .cmx

.ml.cmo:
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

jqlexing.cmo: jqlexing.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(BASEPACKAGES),sedlex.ppx -c $<

clean:
	rm -rf *test *.cm* *.o _build *.log *.cache


depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o \
		$(OML) \
		 > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package sedlex.ppx \
		$(LEXML) \
		 >> .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r \
		$(RML) \
		 >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
