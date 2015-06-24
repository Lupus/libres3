# OASIS_START
# DO NOT EDIT (digest: 46f8bd9984975bd4727bed22d0876cd2)

SETUP = ./setup.exe

build: setup.data $(SETUP)
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data $(SETUP) build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data $(SETUP) build
	$(SETUP) -test $(TESTFLAGS)

all: $(SETUP)
	$(SETUP) -all $(ALLFLAGS)

install: setup.data $(SETUP)
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data $(SETUP)
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data $(SETUP)
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: $(SETUP)
	$(SETUP) -clean $(CLEANFLAGS)

distclean: $(SETUP)
	$(SETUP) -distclean $(DISTCLEANFLAGS)
	$(RM) $(SETUP)

setup.data: $(SETUP)
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure: $(SETUP)
	$(SETUP) -configure $(CONFIGUREFLAGS)

setup.exe: setup.ml
	ocamlfind ocamlopt -o $@ $< || ocamlfind ocamlc -o $@ $< || true
	$(RM) setup.cmi setup.cmo setup.cmx setup.o

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP
headache:
	git ls-files -- src/ test/ | grep -Fv src/sxclient | grep -Fv src/anycache/ | grep -Fv .mldylib | grep -v ml.ab | grep -Fv netcgi_fcgip.ml | grep -Fv src/files | grep -Fv .mllib | grep -Fv .odocl | grep -Fv META | xargs headache -h _header.server 
	git ls-files -- src/sxclient | grep -Fv .mllib | grep -Fv .mldylib | grep -Fv .odocl | grep -Fv https_clientp.ml | grep -Fv META | xargs headache -h _header.client
	git ls-files -- src/anycache | grep -Fv .mllib | grep -Fv .mldylib | grep -Fv .odocl | grep -Fv https_clientp.ml | grep -Fv META | xargs headache -h _header.small

metrics.html: build
	find _build -name '*.ml' -print0 | grep -ZzFv myocamlbuild.ml | xargs -0 mascot -config mascot.conf -html metrics.html 

metrics: build
	find _build -name '*.ml' -print0 | grep -ZzFv myocamlbuild.ml | xargs -0 mascot -config mascot.conf -text -

coverage:
	# clean to make sure all files are built with coverage
	# checking enabled
	$(SETUP) -clean
	$(MAKE) cov

cov:
	$(SETUP) -build -tag pkg_bisect -tag pkg_str -tag syntax_camlp4o -tag syntax_bisect_pp $(BUILDFLAGS)
	rm -f bisect*.out
	$(SETUP) -test $(TESTFLAGS)
	ln -s _build/version.ml version.ml
	bisect-report.opt -html coverage.html bisect*.out -I _build
	rm -f version.ml
	x-www-browser ./coverage.html/index.html
update:
	oasis setup
	sed -i -e 's/bindir)/sbindir)/' setup.ml
reindent:
	git ls-files '*.ml' '*.mli' | grep -Fv myocamlbuild.ml | grep -Fv setup.ml | xargs ocp-indent -i
	$(MAKE) headache
