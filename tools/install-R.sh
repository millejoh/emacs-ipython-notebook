#!/bin/bash

# install R for Actions CI

set -x

WORKDIR=${HOME}/local
UNAME=$(uname -s)
cd $WORKDIR
if [ "x$UNAME" = "xLinux" ] ; then
    if [ ! -d ${WORKDIR}/R ]; then
        wget http://cran.mirrors.hoobly.com/src/base/R-3/R-3.4.1.tar.gz
        tar xvf R-3.4.1.tar.gz
        cd R-3.4.1
        ./configure --prefix=${WORKDIR}/R
        make && make install
        find ${WORKDIR}/R -name R -print
    fi
elif [ "x$UNAME" = "xDarwin" ]; then
    brew list r &>/dev/null || HOMEBREW_NO_AUTO_UPDATE=1 brew install r
fi
mkdir -p ${WORKDIR}/R
echo ".libPaths( c( '${WORKDIR}/R' , .libPaths() ) )" > ${HOME}/.Rprofile
R -e "install.packages('IRkernel', repos='http://cran.mirrors.hoobly.com', lib='${WORKDIR}/R')"
R -e "IRkernel::installspec()"
R --version
