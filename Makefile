SHELL := bash
.ONESHELL:
.PHONY: autogen-deps run-testnet-node run-testnet-ogmios
.SHELLFLAGS := -eu -o pipefail -c

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
