#!/bin/sh

travis logs -i | sed '/^begin 664/,/^end/!d' | uudecode
