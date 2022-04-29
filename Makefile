SHELL := bash
.ONESHELL:
.PHONY: autogen-deps run-testnet-node run-testnet-ogmios
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $$(find ./* -iregex '.*.purs')
ps-entrypoint := Examples.Pkh2Pkh
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

check-format:
	purs-tidy check ${ps-sources}

format:
	purs-tidy format-in-place ${ps-sources}

run-datum-cache-postgres-console:
	nix shell nixpkgs#postgresql -c psql postgresql://ctxlib:ctxlib@localhost:5432
