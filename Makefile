SHELL := bash
.ONESHELL:
.PHONY: autogen-deps run-testnet-node run-testnet-ogmios
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $$(find ./* -iregex '.*.purs')

check-format:
	purs-tidy check ${ps-sources}

format:
	purs-tidy format-in-place ${ps-sources}

run-testnet-node:
	docker run --rm \
	  -e NETWORK=testnet \
	  -v "$$PWD"/.node/socket:/ipc \
	  -v "$$PWD"/.node/data:/data \
	  inputoutput/cardano-node:1.34.0

run-testnet-ogmios:
	ogmios \
		--node-socket "$$CARDANO_NODE_SOCKET_PATH" \
		--node-config "$$CARDANO_NODE_CONFIG"

run-haskell-server:
	nix run -L .#ctl-server:exe:ctl-server

run-datum-cache-postgres:
	docker run -d --rm \
		-e "POSTGRES_USER=ctxlib" \
		-e "POSTGRES_PASSWORD=ctxlib" \
		-e "POSTGRES_DB=ctxlib" \
		-p 127.0.0.1:5432:5432 \
		postgres:13

run-datum-cache-postgres-console:
	nix shell nixpkgs#postgresql -c psql postgresql://ctxlib:ctxlib@localhost:5432

query-testnet-sync:
	cardano-cli query tip --testnet-magic 1097911063
