#!/bin/sh

files="
*.md
*.nix
*.dhall
.eslintrc.json
.mlc_config.json
.tidyrc.json
LICENSE
Makefile
webpack.config.js
package.json
doc/
examples/
fixtures/scripts/
scripts/
src/
templates/ctl-scaffold/*.md
templates/ctl-scaffold/*.nix
templates/ctl-scaffold/*.dhall
templates/ctl-scaffold/index.html
templates/ctl-scaffold/index.js
templates/ctl-scaffold/package.json
templates/ctl-scaffold/webpack.config.js
templates/ctl-scaffold/Makefile
templates/ctl-scaffold/exe/
templates/ctl-scaffold/src/
templates/ctl-scaffold/test/
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
