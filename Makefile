EMACS = emacs
TESTEIN = tools/testein.py

testein-default:
	$(TESTEIN) --clean-elc --load-ert

testein-24:
	$(TESTEIN) --clean-elc -e emacs-snapshot

testein-unit-all:
	$(TESTEIN) --no-func-test --clean-elc --load-ert
	$(TESTEIN) --no-func-test --clean-elc -e emacs-snapshot

interactive-testein-default:
	$(TESTEIN) --clean-elc --load-ert --no-batch

interactive-testein-24:
	$(TESTEIN) --clean-elc -e emacs-snapshot --no-batch

ert-compile: ert-clean
	cd lib/ert/lisp/emacs-lisp/ && \
		$(EMACS) -Q -L . -batch -f batch-byte-compile *.el

ert-clean:
	rm -f lib/ert/lisp/emacs-lisp/*.elc

travis-ci: ert-compile
	$(EMACS) --version
	python --version
	$(TESTEIN) --no-func-test --clean-elc -e $(EMACS)
	tail -n3 log/test-load_messages_batch_$(EMACS).log
