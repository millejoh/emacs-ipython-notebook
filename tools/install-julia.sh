#!/bin/bash

# install julia for Travis CI

set -x

WORKDIR=${HOME}/local
cd $WORKDIR
if [ "x$TRAVIS_OS_NAME" = "xlinux" ] ; then
    if [ ! -d ${WORKDIR}/julia-1.1.0 ]; then
        wget https://julialang-s3.julialang.org/bin/linux/x64/1.1/julia-1.1.0-linux-x86_64.tar.gz
        tar zxvf julia-1.1.0-linux-x86_64.tar.gz
        rm -f julia-1.1.0-linux-x86_64.tar.gz
    fi
    hash
    julia --version
    julia -e 'import Pkg; Pkg.add("IJulia")'
elif [ "x$TRAVIS_OS_NAME" = "xosx" ]; then
    brew cask list julia &>/dev/null || HOMEBREW_NO_AUTO_UPDATE=1 brew cask install julia
    julia --version
    julia -e 'import Pkg; Pkg.add("IJulia")'
fi
