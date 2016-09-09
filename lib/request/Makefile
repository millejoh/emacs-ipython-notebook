CASK ?= cask
EMACS ?= emacs
VIRTUAL_EMACS = ${CASK} exec ${EMACS}

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

# See: cask-elpa-dir

TEST_1 = ${MAKE} EMACS=${EMACS} CASK=${CASK} test-1

.PHONY : test test-all test-1 compile elpa clean clean-elpa clean-elc \
	print-deps before-test travis-ci

test: elpa
	${MAKE} test-3

test-3: test-3-tornado test-3-flask

test-3-tornado:
	EL_REQUEST_TEST_SERVER=tornado ${MAKE} test-2

test-3-flask:
	EL_REQUEST_TEST_SERVER=flask ${MAKE} test-2

# Run test for different backends, for one server.
test-2: test-2-url-retrieve test-2-curl

test-2-url-retrieve:
	EL_REQUEST_BACKEND=url-retrieve ${TEST_1}

test-2-curl:
	EL_REQUEST_BACKEND=curl ${TEST_1}

# Run test without checking elpa directory.
test-1:
	${VIRTUAL_EMACS} -Q -batch \
		-L . -L tests -l tests/test-request.el \
		-f ert-run-tests-batch-and-exit

elpa: ${ELPA_DIR}
${ELPA_DIR}: Cask
	${CASK} install
	touch $@

clean-elpa:
	rm -rf ${ELPA_DIR}

compile: clean-elc elpa
	${VIRTUAL_EMACS} -Q -batch -L . -L tests \
		-f batch-byte-compile *.el */*.el

clean-elc:
	rm -f *.elc */*.elc

clean: clean-elpa clean-elc

print-deps: elpa
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	curl --version
	@echo "------------------------------------------------------------"

before-test: elpa

travis-ci: print-deps test



# Run test against Emacs listed in ${EMACS_LIST}.
# This is for running tests for multiple Emacs versions.
# This is not used in Travis CI.  Usage::
#
#     make EMACS_LIST="emacs emacs-snapshot emacs23" test-all
#
# See: http://stackoverflow.com/a/12110773/727827
#
# Use ${MET_MAKEFLAGS} to do the tests in parallel.
#
#    MET_MAKEFLAGS=-j4
#
# Use ${MET_PRE_TARGETS} to set additional jobs to do before tests.
#
#    MET_PRE_TARGETS=compile

JOBS := $(addprefix job-,${EMACS_LIST})
.PHONY: ${JOBS}

${JOBS}: job-%:
	${MAKE} EMACS=$* clean-elc ${MET_PRE_TARGETS}
	${MAKE} EMACS=$* ${MET_MAKEFLAGS} test

test-all: ${JOBS}



### Package installation
PACKAGE = request.el
PACKAGE_USER_DIR =
TEST_PACKAGE_DIR = dist/test
TEST_INSTALL = ${MAKE} install-dist PACKAGE_USER_DIR=${TEST_PACKAGE_DIR}

install-dist:
	test -d '${PACKAGE_USER_DIR}'
	${EMACS} --batch -Q \
	-l package \
        --eval " \
        (add-to-list 'package-archives \
             '(\"marmalade\" . \"http://marmalade-repo.org/packages/\") t)" \
	--eval '(setq package-user-dir "${PWD}/${PACKAGE_USER_DIR}")' \
	--eval '(package-list-packages)' \
	--eval '(package-install-file "${PWD}/${PACKAGE}")'

test-install:
	rm -rf ${TEST_PACKAGE_DIR}
	mkdir -p ${TEST_PACKAGE_DIR}
	${TEST_INSTALL}
	${TEST_INSTALL} PACKAGE=request-deferred.el
