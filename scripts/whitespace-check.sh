#!/bin/sh

# Check trailing whitespaces

trailing_whitespaces=$(grep -I '[[:blank:]]$' -R \
  $(find ./doc \
    ./examples \
    ./fixtures/scripts \
    ./nix \
    ./scripts \
    ./server \
    ./src \
    ./templates/ctl-scaffold \
    ./test \
    ./*.md \
    ./*.nix \
    ./Makefile \
    ./*.dhall))

if [ -z "$trailing_whitespaces" ]
then
    echo "All source files have no trailing spaces"
else
    echo "Some source files have trailing spaces:"
    echo $trailing_whitespaces
    exit 1
fi

# Check newline on end of a file

no_newline=$(find ./doc \
  ./examples \
  ./fixtures/scripts \
  ./nix \
  ./scripts \
  ./server \
  ./src \
  ./templates/ctl-scaffold \
  ./test \
  ./*.md \
  ./*.nix \
  ./Makefile \
  ./*.dhall \
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
