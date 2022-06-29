SHELL := bash
.ONESHELL:
.PHONY: autogen-deps run-testnet-node run-testnet-ogmios
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd -epurs)
nix-sources := $(shell fd -enix --exclude='spago*')
ps-entrypoint := Examples.Pkh2Pkh
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js

node-ipc = $(shell docker volume inspect cardano-transaction-lib_node-ipc | jq -r '.[0].Mountpoint')

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

check-format:
	@purs-tidy check ${ps-sources} && nixpkgs-fmt --check ${nix-sources}

e2e-test:
	@rm -f chrome-data/SingletonLock && spago test --main Test.E2E

format:
	@purs-tidy format-in-place ${ps-sources}

run-datum-cache-postgres-console:
	@nix shell nixpkgs#postgresql -c psql postgresql://ctxlib:ctxlib@localhost:5432

query-testnet-tip:
	CARDANO_NODE_SOCKET_PATH=${node-ipc}/node.socket cardano-cli query tip \
	  --testnet-magic 1097911063
