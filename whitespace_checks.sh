#!/bin/sh

# Check trailing whitespaces

trailing_whitespaces=$(grep -I '[[:blank:]]$' -Rl . | grep -v -E '.*(spago|.git|dist-newstyle|node_modules|.node|generated-docs|dist|test-data|.spago2nix).*')

if [ -z "$trailing_whitespaces" ]
then
    echo "No trailing spaces"
else
    echo "Has trailing spaces in files: "
    echo $trailing_whitespaces
    exit 1
fi

# Check newline on end of a file

no_newline=$(find . -not -path './.git/*' -not -path './.spago/*' -not -path './output/*' -not -path '*.json' -not -path '*.skey' -not -path '*node_modules*' -not -path '*.node*' -not -path '*generated-docs*' -not -path '*dist*' -not -path '*test-data*' -not -path '*spago2nix*' -type f -exec sh -c 'file -b "{}" | grep -q text' \; -exec sh -c '[ "$(tail -c 1 "{}" | od -An -a | tr -d "[:space:]")" != "nl" ]' \; -print)

if [ -z "$no_newline" ]
then
    echo "Has newlines on the end of each file"
else
    echo "No newline on the end in files: "
    echo $no_newline
    exit 1
fi
