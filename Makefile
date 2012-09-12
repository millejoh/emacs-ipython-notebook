EMACS = emacs

testein-default:
	./testein.py --clean-elc --load-ert

testein-24:
	./testein.py --clean-elc -e emacs-snapshot

testein-unit-all:
	./testein.py --no-func-test --clean-elc --load-ert
	./testein.py --no-func-test --clean-elc -e emacs-snapshot

interactive-testein-default:
	./testein.py --clean-elc --load-ert --no-batch

interactive-testein-24:
	./testein.py --clean-elc -e emacs-snapshot --no-batch

ert-compile: ert-clean
	cd lib/ert/lisp/emacs-lisp/ && \
		$(EMACS) -Q -L . -batch -f batch-byte-compile *.el

ert-clean:
	rm -f lib/ert/lisp/emacs-lisp/*.elc

travis-ci: ert-compile
	$(EMACS) --version
	./testein.py --no-func-test --clean-elc -e $(EMACS)
	tail -n3 test-load_messages_batch_$(EMACS).log
