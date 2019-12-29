#!/bin/bash

# Create virtualenvs for python{2,3} for Travis CI on OSX

set -x

WORKDIR=${HOME}/local

. tools/retry.sh

if [ "x$TRAVIS_OS_NAME" = "xosx" ]; then
    brew list pyenv-virtualenv &>/dev/null || HOMEBREW_NO_AUTO_UPDATE=1 brew install pyenv-virtualenv
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"

    case "${TOXENV}" in
        py27)
            pyenv install -s 2.7.12
            pyenv virtualenv -f 2.7.12 py27
            ;;
        py35)
            pyenv install -s 3.5.2
            pyenv virtualenv -f 3.5.2 py35
            ;;
        py36)
            pyenv install -s 3.6.9
            pyenv virtualenv -f 3.6.9 py36
            ;;
        py37)
            pyenv install -s 3.7.5
            pyenv virtualenv -f 3.7.5 py37
            pyenv rehash
            ;;
    esac
fi
