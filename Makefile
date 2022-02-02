SHELL := bash
.ONESHELL:
.PHONY: autogen-deps run-testnet-node run-testnet-ogmios
.SHELLFLAGS := -eu -o pipefail -c

autogen-deps:
	spago2nix generate \
		&& node2nix -l package-lock.json -d -c node2nix.nix

check-format:
	purs-tidy check "src/**/*.purs" "test/**/*.purs"

format:
	purs-tidy format-in-place "src/**/*.purs" "test/**/*.purs"


run-testnet-node:
	docker run --rm \
	  -e NETWORK=testnet \
	  -v "$$PWD"/.node/socket:/ipc \
	  -v "$$PWD"/.node/data:/data \
	  inputoutput/cardano-node:1.31.0

run-testnet-ogmios:
	ogmios \
		--node-socket "$$CARDANO_NODE_SOCKET_PATH" \
		--node-config "$$CARDANO_NODE_CONFIG"

query-testnet-sync:
	cardano-cli query tip --testnet-magic 1097911063
