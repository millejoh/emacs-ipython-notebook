EMACS=emacs

PACKAGE=markdown-mode

SOURCE=markdown-mode.el
COMPILED=markdown-mode.elc

VERSION=$(shell cat $(SOURCE) | sed -n 's/^;; Version: \(.*\)/\1/p')

TEST_FILES=tests/Makefile tests/*.el tests/*.text tests/*.md

.el.elc:
	$(EMACS) -q -no-site-file -no-init-file -batch -f batch-byte-compile $<

all: $(COMPILED)

.PHONY: dist test

test:
	make -C tests test

clean:
	rm -f $(COMPILED)
	make -C tests clean

dist:
	DIR=$$(mktemp -d -t "$(PACKAGE)"); \
	DESTDIR="$$DIR/$(PACKAGE)-$(VERSION)"; \
	mkdir -p $$DESTDIR; \
	cp -a $(SOURCE) $$DESTDIR; \
	mkdir -p $$DESTDIR/tests; \
	cp -a $(TEST_FILES) $$DESTDIR/tests; \
	tar zcf $(CURDIR)/$(PACKAGE)-$(VERSION).tar.gz -C $$DIR .; \
	rm -r $$DIR; \
	echo "$(PACKAGE)-$(VERSION).tar.gz has been created"

update: $(COMPILED)
	cp -a $(SOURCE) $(COMPILED) $(HOME)/.emacs.d/site-lisp
