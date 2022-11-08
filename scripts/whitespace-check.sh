#!/bin/sh

# Check trailing whitespaces

trailing_whitespaces=$(grep -I '[[:blank:]]$' -R \
    $(find . \
        -not -path '.' \
        -not -path './tmp*' \
        -not -path './.git*' \
        -not -path './.spago*' \
        -not -path './output*' \
        -not -path '*.json' \
        -not -path '*.skey' \
        -not -path '*node_modules*' \
        -not -path '*.node*' \
        -not -path '*.psci_modules*' \
        -not -path '*generated-docs*' \
        -not -path '*dist*' \
        -not -path '*test-data*' \
        -not -path '*spago2nix*' \
        -type d))

if [ -z "$trailing_whitespaces" ]
then
    echo "All source files have no trailing spaces"
else
    echo "Some source files have trailing spaces:"
    echo $trailing_whitespaces
    exit 1
fi

# Check newline on end of a file

no_newline=$(find . \
-not -path './tmp/*' \
-not -path './.git/*' \
-not -path './.spago/*' \
-not -path './output/*' \
-not -path '*.json' \
-not -path '*.skey' \
-not -path '*node_modules*' \
-not -path '*.node*' \
-not -path '*.psci_modules*' \
-not -path '*generated-docs*' \
-not -path '*dist*' \
-not -path '*test-data*' \
-not -path '*spago2nix*' \
-type f -exec sh -c 'file -b "{}" | grep -q text' \; \
-exec sh -c \
    '[ "$(tail -c 1 "{}" | od -An -a | tr -d "[:space:]")" != "nl" ]' \; \
-print)

if [ -z "$no_newline" ]
then
    echo "All source files have newlines at the end"
else
    echo "Not all source files have newlines at the end:"
    echo $no_newline
    exit 1
fi
