#!/usr/bin/env bash

mkdir -p ./html
cd doc;
pandoc -f markdown \
           --standalone \
           --css ../scripts/pandoc.css \
           --highlight-style monochrome \
           --lua-filter=../scripts/readme-link-fixup.lua \
           --lua-filter=../scripts/links-to-html.lua \
           --lua-filter=../scripts/title-from-markdown.lua \
           --template=../scripts/template.html \
           -t html -s ../README.md > ../html/index.html;

for i in ./*.md; do
    pandoc -f markdown \
           --standalone \
           --css ../scripts/pandoc.css \
           --highlight-style monochrome \
           -V geometry:margin=1in \
           --lua-filter=../scripts/links-to-html.lua \
           --lua-filter=../scripts/examples-links-fixup.lua \
           --lua-filter=../scripts/title-from-markdown.lua \
           --include-in-header=../scripts/header.html \
           --template=../scripts/template.html \
           -t html -s "$i" > ../html/"${i%.*}".html;
done;

cp -r ../doc/images ../html/
