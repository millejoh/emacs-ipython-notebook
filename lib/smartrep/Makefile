EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q -batch -L . -eval \
	"(progn \
     (when (version<= \"24.3\" emacs-version) \
     (setq byte-compile-error-on-warn t)) \
     (batch-byte-compile))" smartrep.el
test:
	${CASK} exec ert-runner
clean:
	rm -f smartrep.elc

.PHONY: all compile test clean
