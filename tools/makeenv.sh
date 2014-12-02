#!/bin/sh

env="$1"
req="$2"
activate=$env/bin/activate

if [ -z "$env" -o -z "$req" ]; then
    echo "Usage:"
    echo "    $0 ENVIRONMENT REQUIREMENT"
    exit 1
fi

if [ -e $activate ]; then
    echo "virtualenv $env exists."
else
    echo "Creating virtualenv $env."
    virtualenv $env
fi

. $activate
pip install --quiet --requirement $req
