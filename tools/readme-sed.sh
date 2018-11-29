#!/bin/bash

# by mklement0 https://stackoverflow.com/a/29613573/5132008

# Define sample multi-line literal.
input=`cat`
replace=$(awk '/key.*binding/,EOF { print "   " $0 }' <<<"$input")

# Escape it for use as a Sed replacement string.
IFS= read -d '' -r < <(sed -e ':a' -e '$!{N;ba' -e '}' -e 's/[&/\]/\\&/g; s/\n/\\&/g' <<<"$replace")
replaceEscaped=${REPLY%$'\n'}

# If ok, outputs $replace as is.
sed "/$1/c\\$replaceEscaped" $2
