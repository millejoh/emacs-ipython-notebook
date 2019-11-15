EMACS ?= $(shell which emacs)
SRC=$(shell cask files)
PKBUILD=2.3
ELCFILES = $(SRC:.el=.elc)
ifeq ($(TRAVIS_PULL_REQUEST_SLUG),)
TRAVIS_PULL_REQUEST_SLUG := $(shell git config --global user.name)/$(shell basename `git rev-parse --show-toplevel`)
endif
ifeq ($(TRAVIS_PULL_REQUEST_BRANCH),)
TRAVIS_PULL_REQUEST_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
endif
ifeq ($(TRAVIS),true)
ifeq ($(TRAVIS_PULL_REQUEST_SHA),)
TRAVIS_PULL_REQUEST_SHA := $(shell if git show-ref --quiet --verify origin/$(TRAVIS_PULL_REQUEST_BRANCH) ; then git rev-parse origin/$(TRAVIS_PULL_REQUEST_BRANCH) ; fi))
endif
endif

.DEFAULT_GOAL := test-compile

README.rst: README.in.rst lisp/ein.el
	cask eval "(progn \
	             (add-to-list 'load-path \"./lisp\") \
	             (load \"ein-notebook\") \
	             (describe-minor-mode \"ein:notebook-mode\") \
	             (with-current-buffer \"*Help*\" (princ (buffer-string))))" 2>/dev/null \
	| tools/readme-sed.sh "KEYS NOTEBOOK" README.in.rst "key.*binding" > README.rst0
	sed "/CI VERSION/c"`grep -o 'emacs-[0-9][.0-9]*' .travis.yml | sort -n | head -1 | grep -o '[.0-9]*'` README.rst0 > README.rst1
	grep ';;' lisp/ein.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.rst1 > README.rst0
	cask eval "(progn \
	             (add-to-list 'load-path \"./lisp\") \
	             (load \"ein-connect\") \
	             (describe-minor-mode \"ein:connect-mode\") \
	             (with-current-buffer \"*Help*\" (princ (buffer-string))))" 2>/dev/null \
	| tools/readme-sed.sh "KEYS CONNECT" README.rst0 "key.*binding" > README.rst
	rm README.rst0 README.rst1

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"ein\" \"./lisp\")"

.PHONY: clean
clean:
	cask clean-elc
	rm -rf test/test-install
	rm -rf log
	rm -f features/Untitled*.ipynb
	rm -f test/Untitled*.ipynb

.PHONY: dist-clean
dist-clean: clean
	rm -rf dist


.PHONY: test-compile
test-compile: clean autoloads
	cask install
	! (cask eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: quick
quick: test-compile test-ob-ein-recurse test-unit

.PHONY: test-jupyterhub
test-jupyterhub: test-compile
# jupyterhub slightly temperamental with json-readtable-error
# seems to be affecting ob-ipython too but probably my bug.. just need to find it
	-cask exec ecukes --tags @jupyterhub --reporter magnars

.PHONY: test
test: quick test-int

.PHONY: test-unpoly
test-poly:
	cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-poly.el test/test-func.el
	cp test/test-poly.el features/support/test-poly.el
	cask exec ecukes --reporter magnars ; (ret=$$? ; rm -f features/support/test-poly.el && exit $$ret)

.PHONY: test-int
test-int:
	cask exec ecukes --reporter magnars

.PHONY: test-unit
test-unit:
	cask exec ert-runner -L ./lisp -L ./test -l test/testein.el test/test-ein*.el
	cask exec ert-runner -L ./lisp -L ./test test/test-uncompiled.el

.PHONY: test-ob-ein-recurse
test-ob-ein-recurse:
	cask eval "(progn (require 'cl) (custom-set-variables (quote (org-babel-load-languages (quote ((emacs-lisp . t) (ein . t)))))) (org-version))"

.PHONY: test-install
test-install:
	mkdir -p test/test-install
	if [ ! -s "test/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd test/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd test/test-install ; tar xfz $(PKBUILD).tar.gz
	cd test/test-install ; rm -f $(PKBUILD).tar.gz
	cd test/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p test/test-install/recipes
	cd test/test-install/recipes ; curl -sLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/ein
	! ( $(EMACS) -Q --batch -L test/test-install/package-build-$(PKBUILD) \
	--eval "(require 'package-build)" \
	--eval "(require 'subr-x)" \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"ein\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(TRAVIS_PULL_REQUEST_SLUG)\") \
	               (my-branch \"$(TRAVIS_PULL_REQUEST_BRANCH)\") \
	               (my-commit \"$(TRAVIS_PULL_REQUEST_SHA)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"ein*.tar\"))))" 2>&1 | egrep -a "Error: " )

.PHONY: dist
dist:
	rm -rf dist
	cask package

.PHONY: install
install: dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/ein*.tar\")))"
