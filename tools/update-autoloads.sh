#!/bin/sh

# Update ein-loaddefs.el

if [ -z "$EMACS" ]; then
    EMACS="$(which emacs)"
fi

# To omit slashes in the second FILE argument, need to go to the
# directory.
cd lisp || exit $?

$EMACS -Q -batch --eval \
    "(setq generated-autoload-file \"$(pwd)/ein-loaddefs.el\")" \
    -f batch-update-autoloads .
