#!/bin/bash

# Install evm for Travis CI
# or if already installed, then check for updates
# Author: gonewest818 https://github.com/clojure-emacs/cider/pull/2139
set -x

WORKDIR=${HOME}/local
EVMDIR=$WORKDIR/evm

. tools/retry.sh

if [ -d $EVMDIR ]
then
    cd $EVMDIR
    git pull origin master
else
    git clone https://github.com/rejeep/evm.git $EVMDIR
fi
