# Makefile
# --------
# Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# Generic Makefile for oasis project

# Set to setup.exe for the release
SETUP := setup-dev.exe

# Default rule
default: build

# Setup for the development version
setup-dev.exe: _oasis setup.ml
	grep -v '^#' setup.ml > setup_dev.ml
	ocamlfind ocamlopt -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup_dev.ml || \
	ocamlfind ocamlc -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup_dev.ml || true
	rm -f setup_dev.*

# Setup for the release
setup.exe: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.obj setup.cmo

build: $(SETUP) setup.data
	./$(SETUP) -build -j 0 $(BUILDFLAGS)

doc: $(SETUP) setup.data build
	./$(SETUP) -doc $(DOCFLAGS)

doc-api: $(SETUP) setup.data build
	./$(SETUP) -build lwt-api.docdir/index.html

test: $(SETUP) setup.data build
	./$(SETUP) -test $(TESTFLAGS)

all: $(SETUP)
	./$(SETUP) -all $(ALLFLAGS)

install: $(SETUP) setup.data
	./$(SETUP) -install $(INSTALLFLAGS)

uninstall: $(SETUP) setup.data
	./$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: $(SETUP) setup.data
	./$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: $(SETUP)
	./$(SETUP) -clean $(CLEANFLAGS)

distclean: $(SETUP)
	./$(SETUP) -distclean $(DISTCLEANFLAGS)

configure: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

setup.data: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: default build doc test all install uninstall reinstall clean distclean configure runtime examples toplevel

OCB= ./$(SETUP) -build -j 0 #-classic-display

examples: $(SETUP)
	$(OCB) examples/all.otarget

toplevel: $(SETUP)
	$(OCB) toplevel/toplevel_expunge.js

tests: $(SETUP) check_phantomjs
	$(OCB) tests/tests.js

TESTS_LOG = $(patsubst %.ml,%.jslog,$(wildcard tests/test_*.ml))
phantomtests: check_phantomjs
	$(OCB) $(TESTS_LOG)

PHANTOMJSERROR="You need phantomjs in your PATH to run this"
check_phantomjs:
	@if which phantomjs; then echo ""; else \
		echo $(PHANTOMJSERROR); exit 1; \
	fi

regenerate_oasis_files::
	oasis setup-clean -replace-sections
	oasis setup -setup-update dynamic

NAME=`oasis query Name 2> /dev/null`
VERSION=`oasis query Version 2> /dev/null`

dist:
	rm -rf /tmp/${NAME}-${VERSION} &&\
	cd /tmp &&\
	git clone https://github.com/ocsigen/js_of_ocaml.git ${NAME}-${VERSION} &&\
	tar zcvf ${NAME}-${VERSION}.tar.gz ${NAME}-${VERSION} --exclude benchmarks --exclude _darcs --exclude .git --exclude tests
