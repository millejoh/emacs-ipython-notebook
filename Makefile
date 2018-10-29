SRC=$(shell cask files)
ELCFILES = $(SRC:.el=.elc)

.DEFAULT_GOAL := test-compile

.PHONY: install
install:
	rm -rf dist/
	cask package
	emacs -Q --batch --eval "(package-initialize)" --eval "(package-install-file (car (file-expand-wildcards \"dist/ein*.tar\")))"

.PHONY: autoloads
autoloads:
	emacs -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"ein\" \"./lisp\")"

.PHONY: clean
clean:
	cask clean-elc

env-ipy.%:
	tools/makeenv.sh env/ipy.$* tools/requirement-ipy.$*.txt

.PHONY: test-compile
test-compile: clean autoloads
	! ( cask build 2>&1 | awk '{if (/^ /) { gsub(/^ +/, " ", $$0); printf "%s", $$0 } else { printf "\n%s", $$0 }}' | egrep -a "not known|Error|free variable|error for|Use of gv-ref" )
	cask clean-elc

.PHONY: quick
quick: test-compile test-unit

.PHONY: test
test: quick test-int

.PHONY: test-int
test-int:
	cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-func.el
	cask exec ecukes

.PHONY: test-unit
test-unit:
	cask exec ert-runner -L ./lisp -L ./test -l test/testein.el test/test-ein*.el
