SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

hs-sources := $$(find . -iregex '.*.hs')

format:
	@fourmolu -i ${hs-sources} -o -XTypeApplications -o -XImportQualifiedPost

check-format:
	@fourmolu --mode check ${hs-sources} -o -XTypeApplications \
		-o -XImportQualifiedPost
