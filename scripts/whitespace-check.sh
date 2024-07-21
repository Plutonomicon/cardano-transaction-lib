#!/bin/sh

# Checks that files don't have trailing whitespaces in them

files="
*.md
*.nix
*.dhall
eslint.config.js
.mlc_config.json
.tidyrc.json
LICENSE
Makefile
webpack.config.cjs
package.json
doc/*.md
examples/
fixtures/scripts/
scripts/
src/
esbuild/
templates/ctl-scaffold/*.md
templates/ctl-scaffold/*.nix
templates/ctl-scaffold/*.dhall
templates/ctl-scaffold/package.json
templates/ctl-scaffold/webpack.config.cjs
templates/ctl-scaffold/Makefile
templates/ctl-scaffold/exe/
templates/ctl-scaffold/src/
templates/ctl-scaffold/test/
templates/ctl-scaffold/esbuild/
test/
"

trailing_whitespaces=$(grep -I '[[:blank:]]$' -R $files)

if [ -z "$trailing_whitespaces" ]
then
    echo "All source files have no trailing spaces"
else
    echo "Some source files have trailing spaces:"
    echo $trailing_whitespaces
    exit 1
fi

# Check newline on end of a file

no_newline=$(find $files \
-type f \
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
