#!/bin/bash

# Install cask for Travis CI
# or if already installed, then check for updates
# Author: gonewest818 https://github.com/clojure-emacs/cider/pull/2139

set -x

WORKDIR=${HOME}/local
CASKDIR=$WORKDIR/cask

. tools/retry.sh

update_elpa_keys() {
    mkdir -p $HOME/.emacs.d/elpa/gnupg || true
    chmod 700 $HOME/.emacs.d/elpa/gnupg
    GPG=gpg
    if which gpg2 ; then GPG=gpg2 ; fi
    travis_retry ${GPG} --keyserver hkp://pool.sks-keyservers.net:80 --homedir $HOME/.emacs.d/elpa/gnupg --recv-keys 066DAFCB81E42C40
    mkdir -p $(cask package-directory) || true
    mkdir -p $HOME/.cask || true
    rsync -azSHe ssh $HOME/.cask $(dirname $(dirname $(dirname $(cask package-directory))))
    rsync -azSHe ssh $HOME/.emacs.d/elpa/gnupg $(cask package-directory)
}

cask_upgrade_cask_or_reset() {
    cask upgrade-cask || { rm -rf $HOME/.emacs.d/.cask && false; }
}

cask_install_or_reset() {
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
update_elpa_keys
travis_retry cask_upgrade_cask_or_reset
travis_retry cask_install_or_reset && touch elpa-emacs
