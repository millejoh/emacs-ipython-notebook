#!/bin/bash

# Install cask for Travis CI
# or if already installed, then check for updates
# Author: gonewest818 https://github.com/clojure-emacs/cider/pull/2139

set -x

WORKDIR=${HOME}/local
CASKDIR=$WORKDIR/cask

. tools/retry.sh

cask_upgrade_cask_or_reset() {
    cask upgrade-cask || { rm -rf $HOME/.emacs.d/.cask && false; }
}

cask_install_or_reset() {
    rsync -azSHe ssh $HOME/.cask $(dirname $(dirname $(dirname $(cask package-directory))))
    chmod 700 $HOME/.emacs.d/elpa
    gpg2 --keyserver hkp://pool.sks-keyservers.net:80 --homedir $HOME/.emacs.d/elpa --recv-keys 066DAFCB81E42C40
    mkdir -p $(cask package-directory) || true
    cp -R $HOME/.emacs.d/elpa/gnupg $(cask package-directory)
    cask install </dev/null
    find $(cask package-directory)/archives -print | xargs ls -l
    find $(cask package-directory)/gnupg -print | xargs ls -l
    # travis cache
    rsync -azSHe ssh $(dirname $(dirname $(cask package-directory))) $HOME/
}

# Bootstrap the cask tool and its dependencies
if [ ! -d $CASKDIR ] ; then
    git clone https://github.com/cask/cask.git $CASKDIR
fi

# Install dependencies for cider as descriped in ./Cask
# Effect is identical to "make elpa", but here we can retry
# in the event of network failures.
travis_retry cask_upgrade_cask_or_reset
travis_retry cask_install_or_reset && touch elpa-emacs
