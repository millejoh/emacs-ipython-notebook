#!/bin/bash
cd build/html/
git add .
if [ -n "$(git ls-files --deleted)" ]
then
    git ls-files --deleted | xargs git rm
fi
git commit -m "Update"
