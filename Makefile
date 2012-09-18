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

ert-compile: ert-clean
	cd lib/ert/lisp/emacs-lisp/ && \
		$(EMACS) -Q -L . -batch -f batch-byte-compile *.el

ert-clean:
	rm -f lib/ert/lisp/emacs-lisp/*.elc

env-ipy.0.13.0:
	tools/makeenv.sh env/ipy.0.13.0 tools/requirement-ipy.0.13.0.txt

env-ipy.0.12.1:
	tools/makeenv.sh env/ipy.0.12.1 tools/requirement-ipy.0.12.1.txt

env-ipy.0.12.0:
	tools/makeenv.sh env/ipy.0.12.0 tools/requirement-ipy.0.12.0.txt

travis-ci: ert-compile env-ipy.$(IPY_VERSION)
	$(EMACS) --version
	python --version
	$(TESTEIN) --clean-elc -e $(EMACS) \
		--ipython env/ipy.$(IPY_VERSION)/bin/ipython
