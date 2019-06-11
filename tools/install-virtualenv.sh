#!/bin/bash

# Create virtualenvs for python{2,3} for Travis CI on OSX

set -x

WORKDIR=${HOME}/local

. tools/retry.sh

if [ "x$TRAVIS_OS_NAME" = "xosx" ]; then
    brew list pyenv-virtualenv || HOMEBREW_NO_AUTO_UPDATE=1 brew install pyenv-virtualenv

    case "${TOXENV}" in
        py27)
            pyenv install -s 2.7.12
            pyenv virtualenv -f 2.7.12 py27
            ;;
        py35)
            pyenv install -s 3.5.2
            pyenv virtualenv -f 3.5.2 py35
            ;;
    esac
fi
