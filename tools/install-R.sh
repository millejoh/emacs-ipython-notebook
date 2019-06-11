#!/bin/bash

# install R for Travis CI

set -x

WORKDIR=${HOME}/local
cd $WORKDIR
if [ "x$TRAVIS_OS_NAME" = "xlinux" ] ; then
    if [ ! -d ${WORKDIR}/R ]; then
        wget http://cran.mirrors.hoobly.com/src/base/R-3/R-3.4.1.tar.gz
        tar xvf R-3.4.1.tar.gz
        cd R-3.4.1
        ./configure --prefix=${WORKDIR}/R
        make && make install
    fi
    R -e "install.packages('IRkernel', repos='http://cran.mirrors.hoobly.com')"
    R -e "IRkernel::installspec()"
elif [ "x$TRAVIS_OS_NAME" = "xosx" ]; then
    brew list r &>/dev/null || HOMEBREW_NO_AUTO_UPDATE=1 brew install r
    R -e "install.packages('IRkernel', repos='http://cran.mirrors.hoobly.com')"
    R -e "IRkernel::installspec()"
fi
R --version
