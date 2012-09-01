#!/bin/sh

# Update ein-loaddefs.el
#
#   --commit  Automatically commit updated changes in ein-loaddefs.el

if [ -z "$EMACS" ]; then
    EMACS="$(which emacs)"
fi

# To omit slashes in the second FILE argument, need to go to the
# directory.
cd lisp || exit $?

$EMACS -Q -batch --eval \
    "(setq generated-autoload-file \"$(pwd)/ein-loaddefs.el\")" \
    -f batch-update-autoloads .

if [ "$1" = "--commit" ]; then
    git commit --message "Update ein-loaddefs.el" ein-loaddefs.el
fi
