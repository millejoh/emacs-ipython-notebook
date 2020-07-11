export EMACS ?= $(shell which emacs)
export CASK := $(shell which cask)
TEST_VERSION := $(shell python -c "from yq import cli; import sys; sys.exit(cli())" .jobs.build.strategy.matrix.emacs_version .github/workflows/test.yml | jq .[] | egrep "[0-9]" | sort -n | head -1)
ifeq ($(CASK),)
$(error Please install CASK at https://cask.readthedocs.io/en/latest/guide/installation.html)
endif
CASK_DIR := $(shell EMACS=$(EMACS) $(CASK) package-directory || exit 1)
SRC=$(shell $(CASK) files)
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

README.rst: README.in.rst lisp/ein.el lisp/ein-notebook.el
	$(CASK) eval "(progn \
	             (add-to-list 'load-path \"./lisp\") \
	             (load \"ein-notebook\") \
	             (describe-minor-mode \"ein:notebook-mode\") \
	             (with-current-buffer \"*Help*\" (princ (buffer-string))))" 2>/dev/null \
	| tools/readme-sed.sh "KEYS NOTEBOOK" README.in.rst "key.*binding" > README.rst0
	perl -ne "s/^((\s*)C-c C-c(\s+).*)$$/\$${1}\n\$${2}C-u C-c C-c    \$${3}ein:worksheet-execute-all-cells/; print" README.rst0 > README.rst1
	perl -ne "s/^(\s*)\S+.*CI VERSION.*$$/\$${1}$(TEST_VERSION)/; print" README.rst1 > README.rst0
	grep ';;' lisp/ein.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.rst0 > README.rst
	rm README.rst0 README.rst1

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"ein\" \"./lisp\")"

.PHONY: clean
clean:
	$(CASK) clean-elc
	rm -rf test/test-install
	rm -rf log
	rm -f features/Untitled*.ipynb
	rm -f features/Renamed.ipynb
	rm -rf features/test-repo

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: test-compile
test-compile: clean autoloads
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)

.PHONY: quick
quick: $(CASK_DIR) test-compile test-ob-ein-recurse test-unit

.PHONY: test
test: quick test-int

.PHONY: test-int
test-int:
	$(CASK) exec ecukes --reporter magnars

.PHONY: test-unit
test-unit:
	$(CASK) exec ert-runner -L ./lisp -L ./test -l test/testein.el test/test-ein*.el

.PHONY: test-ob-ein-recurse
test-ob-ein-recurse:
	$(CASK) eval "(progn (require 'cl) (custom-set-variables (quote (org-babel-load-languages (quote ((emacs-lisp . t) (ein . t)))))) (org-version))"

.PHONY: travis-install
travis-install:
	mkdir -p test/test-install
	if [ ! -s "test/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd test/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd test/test-install ; tar xfz $(PKBUILD).tar.gz
	cd test/test-install ; rm -f $(PKBUILD).tar.gz
	cd test/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p test/test-install/recipes
	cd test/test-install/recipes ; curl -sLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/ein
	$(EMACS) -Q --batch -L test/test-install/package-build-$(PKBUILD) \
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
	--eval "(let ((version (package-build--checkout rcp))) \
	           (delete-directory (expand-file-name (concat \"ein-\" version) package-user-dir) t) \
	           (package-build--package rcp version))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"ein*.tar\"))))" 2>&1 | tee /tmp/test-install.out
	! ( egrep -a "Error: " /tmp/test-install.out )

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean autoloads
	$(CASK) package

.PHONY: backup-melpa
backup-melpa:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval \
	  "(with-temp-buffer \
	    (insert-file-contents-literally (car (file-expand-wildcards \"dist/ein*.tar\"))) \
	    (tar-mode) \
	    (let* ((my-desc (package-tar-file-info)) \
	           (name (package-desc-name my-desc)) \
	           (other-pkgs (cdr (assq name package-alist)))) \
	      (when other-pkgs \
	        (mapcar (lambda (odesc) \
	                  (let* ((odir (package-desc-dir odesc)) \
	                         (parent (file-name-directory odir)) \
	                         (leaf (file-name-nondirectory odir))) \
	                    (if (equal (package-desc-version my-desc) \
	                               (package-desc-version odesc)) \
	                        (delete-directory odir t) \
	                      (rename-file odir \
	                                   (expand-file-name (format \"BACKUP-%s\" leaf) parent) \
	                                   t)))) \
	                other-pkgs))))"

.PHONY: install
install: dist backup-melpa
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/ein*.tar\")))"
