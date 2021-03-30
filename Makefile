
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
PACKAGES=bos,fmt,camlp5.extprint,camlp5.extend,camlp5.pprintf,pcre,yaml,pa_ppx.deriving_plugins.std,pa_ppx.base.link,pa_ppx.runtime,pa_ppx.testutils

OBJ=lazy_reclist.cmo jqutil.cmo jqtypes.cmo jqparse0.cmo jq_examples.cmo

all: $(OBJ)

test:: all
	mkdir -p _build

jqparse0.cmo: jqparse0.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.SUFFIXES: .mll .ml .cmo .cmx

.ml.cmo:
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

clean:
	rm -rf *test *.cm* *.o _build *.log *.cache


depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o \
		lazy_reclist.ml jqutil.cmo \
		 > .depend.NEW
	mv .depend.NEW .depend

-include .depend
