SHELL := bash
.ONESHELL:
.PHONY: autogen-deps run-testnet-node run-testnet-ogmios
.SHELLFLAGS := -eu -o pipefail -c

autogen-deps:
	spago2nix generate \
		&& node2nix -l package-lock.json \
		&& mv default.nix node2nix.nix \
		&& git restore default.nix
