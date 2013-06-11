EMACS ?= emacs
IPYTHON = env/ipy.$(IPY_VERSION)/bin/ipython
IPY_VERSION = 0.13.0
TESTEIN = tools/testein.py
TESTEIN_OPTS =
PKG_INFO = \
grep '^Version' \
env/ipy.$(IPY_VERSION)/lib/python*/site-packages/*.egg-info/PKG-INFO \
| sed -r 's%.*/site-packages/(.*)-py.*\.egg-info/.*:Version: (.*)$$%\1\t\2%'

testein: test-requirements
	${MAKE} testein-1

interactive-testein: test-requirements
	${MAKE} TESTEIN_OPTS="--no-batch" testein-1

clean: ert-clean
	rm -f lisp/*.elc

purge: clean
	rm -rf env log

pkg-info:
	@echo "**************************************************"
	@echo "Installed Python Packages"
	$(PKG_INFO)

submodule:
	git submodule update --init

ERT_DIR = lib/ert/lisp/emacs-lisp
ert-compile: submodule ert-clean log
	$(EMACS) -Q -batch -L $(ERT_DIR) \
		-f batch-byte-compile $(ERT_DIR)/*.el 2> log/ert-compile.log

ert-clean:
	rm -f lib/ert/lisp/emacs-lisp/*.elc

env-ipy.%:
	tools/makeenv.sh env/ipy.$* tools/requirement-ipy.$*.txt

log:
	mkdir log

test-requirements: ert-compile env-ipy.$(IPY_VERSION)
	${MAKE} pkg-info

travis-ci-testein: test-requirements
	${MAKE} testein-2

testein-2: testein-2-url-retrieve testein-2-curl

testein-2-curl:
	EL_REQUEST_BACKEND=curl ${MAKE} testein-1

testein-2-url-retrieve:
	EL_REQUEST_BACKEND=url-retrieve ${MAKE} testein-1

testein-1:
	$(EMACS) --version
	python --version
	env/ipy.$(IPY_VERSION)/bin/ipython --version
	$(TESTEIN) --clean-elc -e $(EMACS) \
		--ipython $(IPYTHON) ${TESTEIN_OPTS}

travis-ci-zeroein:
	$(EMACS) --version
	EMACS=$(EMACS) lisp/zeroein.el -batch
	rm -rf lib/*
	EMACS=$(EMACS) lisp/zeroein.el -batch
