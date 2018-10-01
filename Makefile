EMACS ?= $(shell which emacs)
IPYTHON = env/ipy.$(IPY_VERSION)/bin/ipython
IPY_VERSION = 5.8.0
SRC=$(shell cask files)
ELCFILES = $(SRC:.el=.elc)

.PHONY: loaddefs
loaddefs:
	sh tools/update-autoloads.sh

.PHONY: clean
clean:
	cask clean-elc

env-ipy.%:
	tools/makeenv.sh env/ipy.$* tools/requirement-ipy.$*.txt

.PHONY: test
test: test-unit test-int

.PHONY: test-int
test-int:
	cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-func.el
	cask exec ecukes

.PHONY: test-unit
test-unit:
	cask exec ert-runner -L ./lisp -L ./test -l test/testein.el test/test-ein*.el

travis-ci-zeroein:
	$(EMACS) --version
	EMACS=$(EMACS) lisp/zeroein.el -batch
	rm -rf lib/*
	EMACS=$(EMACS) lisp/zeroein.el -batch
