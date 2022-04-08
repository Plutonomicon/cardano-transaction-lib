SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

format:
	fourmolu -i $$(find . -iregex '.*.hs') \
	  -o -XTypeApplications -o -XImportQualifiedPost
