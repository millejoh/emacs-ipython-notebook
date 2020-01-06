#!/bin/bash

# install julia for Actions CI

set -ex

WORKDIR=${HOME}/local
UNAME=$(uname -s)
cd $WORKDIR
if [ "x$UNAME" = "xLinux" ] ; then
    if [ ! -d ${WORKDIR}/julia-1.3.1 ]; then
        wget https://julialang-s3.julialang.org/bin/linux/x64/1.3/julia-1.3.1-linux-x86_64.tar.gz
        tar zxvf julia-1.3.1-linux-x86_64.tar.gz
        rm -f julia-1.3.1-linux-x86_64.tar.gz
    fi
    hash
    julia --version
    julia -e 'import Pkg; Pkg.add("IJulia")'
elif [ "x$UNAME" = "xDarwin" ]; then
    brew update
    brew cask list julia &>/dev/null || HOMEBREW_NO_AUTO_UPDATE=1 brew cask install julia
    julia --version
    julia -e 'import Pkg; Pkg.add("IJulia")'
fi
