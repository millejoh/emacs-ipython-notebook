EMACS = emacs
IPYTHON = ipython
IPY_VERSION = 0.13.0
TESTEIN = tools/testein.py

testein-default:
	$(TESTEIN) --clean-elc --ipython $(IPYTHON)

testein-24:
	$(TESTEIN) --clean-elc -e emacs-snapshot --ipython $(IPYTHON)

testein-unit-all:
	$(TESTEIN) --no-func-test --clean-elc --load-ert
	$(TESTEIN) --no-func-test --clean-elc -e emacs-snapshot

interactive-testein-default:
	$(TESTEIN) --clean-elc --load-ert --no-batch --ipython $(IPYTHON)

interactive-testein-24:
	$(TESTEIN) --clean-elc -e emacs-snapshot --no-batch --ipython $(IPYTHON)

ERT_DIR = lib/ert/lisp/emacs-lisp
ert-compile: ert-clean log
	$(EMACS) -Q -batch -L $(ERT_DIR) \
		-f batch-byte-compile $(ERT_DIR)/*.el 2> log/ert-compile.log

ert-clean:
	rm -f lib/ert/lisp/emacs-lisp/*.elc

env-ipy.dev:
	tools/makeenv.sh env/ipy.dev tools/requirement-ipy.dev.txt

env-ipy.0.13.0:
	tools/makeenv.sh env/ipy.0.13.0 tools/requirement-ipy.0.13.0.txt

env-ipy.0.12.1:
	tools/makeenv.sh env/ipy.0.12.1 tools/requirement-ipy.0.12.1.txt

env-ipy.0.12.0:
	tools/makeenv.sh env/ipy.0.12.0 tools/requirement-ipy.0.12.0.txt

env-clean:
	rm -rf env

log:
	mkdir log

log-clean:
	rm -rf log

travis-ci-testein: ert-compile env-ipy.$(IPY_VERSION)
	$(EMACS) --version
	python --version
	env/ipy.$(IPY_VERSION)/bin/ipython --version
	$(TESTEIN) --clean-elc -e $(EMACS) \
		--ipython env/ipy.$(IPY_VERSION)/bin/ipython

travis-ci-zeroein:
	$(EMACS) --version
	EMACS=$(EMACS) lisp/zeroein.el -batch
	rm -rf lib/*
	EMACS=$(EMACS) lisp/zeroein.el -batch
