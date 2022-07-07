SHELL := bash
.ONESHELL:
.PHONY: run-dev run-build check-format format run-datum-cache-postgres-console query-testnet-tip clean check-explicit-exports
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd -epurs)
nix-sources := $(shell fd -enix --exclude='spago*')
hs-sources := $(shell fd . './server/src' './server/exe' -ehs)
ps-entrypoint := Examples.AlwaysSucceeds
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js

node-ipc = $(shell docker volume inspect cardano-transaction-lib_node-ipc | jq -r '.[0].Mountpoint')

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

check-explicit-exports: $(eval SHELL:=/bin/bash)
	@[ -z "$$(grep -rn '(\.\.)' ./src ./test ./examples)" ] || \
		(echo "Use explicit exports:" && \
		grep -rn '(\.\.)' ./src ./test ./examples)

check-format:
	@purs-tidy check ${ps-sources}
	nixpkgs-fmt --check ${nix-sources}
	fourmolu -m check -o -XTypeApplications -o -XImportQualifiedPost ${hs-sources}

format:
	@purs-tidy format-in-place ${ps-sources}
	nixpkgs-fmt ${nix-sources}
	fourmolu -m inplace -o -XTypeApplications -o -XImportQualifiedPost ${hs-sources}

run-datum-cache-postgres-console:
	@nix shell nixpkgs#postgresql -c psql postgresql://ctxlib:ctxlib@localhost:5432

query-testnet-tip:
	CARDANO_NODE_SOCKET_PATH=${node-ipc}/node.socket cardano-cli query tip \
	  --testnet-magic 1097911063

clean:
	@ rm -rf dist-newstyle || true
	@ rm -r .psc-ide-port || true
	@ rm -rf .psci_modules || true
	@ rm -rf .spago || true
	@ rm -rf .spago2nix || true
	@ rm -rf node_modules || true
	@ rm -rf output || true
