#!/bin/bash

# Check trailing whitespaces

trailing_whitespaces=$(grep -I '[[:blank:]]$' -Rl . | grep -v -E '.*(spago|dist-newstyle).*')

if [ -z "$trailing_whitespaces" ]
then
    echo "No trailing spaces"
else
    echo "Has trailing spaces in files: "
    echo $trailing_whitespaces
    exit 1
fi

# Check newline on end of a file

no_newline=$(find . -not -path './.git/*' -not -path './.spago/*' -not -path './output/*' -not -path '*.json' -not -path '*dist-newstyle*' -type f -exec sh -c 'file -b "{}" | grep -q text' \; -exec sh -c '[ "$(tail -c 1 "{}" | od -An -a | tr -d "[:space:]")" != "nl" ]' \; -print)
echo $no_newline

if [ -z "$no_newline" ]
then
    echo "No newline on end of a file absence"
else
    echo "Has newline on end of a file absence in files: "
    echo $no_newline
    exit 1
fi
