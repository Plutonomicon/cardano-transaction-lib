SHELL := bash
.ONESHELL:
.PHONY: esbuild-bundle esbuild-serve webpack-bundle webpack-serve check-format format query-testnet-tip clean check-explicit-exports spago-build create-bundle-entrypoint create-html-entrypoint delete-bundle-entrypoint
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
js-sources := $(shell fd --no-ignore-parent -ejs -ecjs)

### Bundler setup

# The main Purescript module
ps-entrypoint := Ctl.Examples.ByUrl
# The entry point function in the main PureScript module
ps-entrypoint-function := main
# Whether to bundle for the browser
browser-runtime := 1 # Use "1" for true and "" for false

preview-node-ipc = $(shell docker volume inspect store_node-preview-ipc | jq -r '.[0].Mountpoint')
preprod-node-ipc = $(shell docker volume inspect store_node-preprod-ipc | jq -r '.[0].Mountpoint')
serve-port := 4008

spago-build:
	@spago build

create-bundle-entrypoint:
	@mkdir -p dist/
	@echo 'import("../output/${ps-entrypoint}/index.js").then(m => m.${ps-entrypoint-function}());' > ./dist/entrypoint.js

delete-bundle-entrypoint:
	@rm -f ./dist/entrypoint.js

create-html-entrypoint:
	@mkdir -p dist/
	@cat << EOF > dist/index.html
	<!DOCTYPE html>
	<html>
	  <body><script type="module" src="./index.js"></script></body>
	</html>
	EOF

esbuild-bundle: spago-build create-bundle-entrypoint
	@mkdir -p dist/
	BROWSER_RUNTIME=${browser-runtime} node esbuild/bundle.js ./dist/entrypoint.js dist/index.js
	@make delete-bundle-entrypoint

esbuild-serve: spago-build create-bundle-entrypoint create-html-entrypoint
	BROWSER_RUNTIME=1 node esbuild/serve.js ./dist/entrypoint.js dist/index.js dist/ ${serve-port}

webpack-bundle: spago-build create-bundle-entrypoint
	BROWSER_RUNTIME=${browser-runtime} webpack --mode=production \
		-o dist/ --env entry=./dist/entrypoint.js
	@make delete-bundle-entrypoint

webpack-serve: spago-build create-bundle-entrypoint create-html-entrypoint
	BROWSER_RUNTIME=1 webpack-dev-server --progress \
		--port ${serve-port} \
		-o dist/ --env entry=./dist/entrypoint.js

.ONESHELL:
check-explicit-exports:
	@if grep -rn '(\.\.)' ${ps-sources}; then
		echo "Use explicit imports/exports ^"
		exit 1
	else
		echo "All imports/exports are explicit"
	fi

check-examples-imports:
	bash ./scripts/examples-imports-check.sh

check-whitespace:
	bash ./scripts/whitespace-check.sh

check-format: check-explicit-exports check-examples-imports check-whitespace
	@purs-tidy check ${ps-sources}
	@nixpkgs-fmt --check ${nix-sources}
	@prettier --loglevel warn -c ${js-sources}
	@eslint --quiet ${js-sources} --parser-options 'sourceType: module'

format:
	@purs-tidy format-in-place ${ps-sources}
	nixpkgs-fmt ${nix-sources}
	prettier -w ${js-sources}
	doctoc CHANGELOG.md README.md doc/*.md --github --notitle
	make check-explicit-exports
	make check-examples-imports
	make check-whitespace

query-preview-testnet-tip:
	CARDANO_NODE_SOCKET_PATH=${preview-node-ipc}/node.socket cardano-cli query tip \
	  --testnet-magic 2

query-preprod-testnet-tip:
	CARDANO_NODE_SOCKET_PATH=${preprod-node-ipc}/node.socket cardano-cli query tip \
	  --testnet-magic 1

run-ci-actions:
	nix build -L .#checks.x86_64-linux.formatting-check
	nix build -L .#checks.x86_64-linux.template-deps-json
	nix build -L .#checks.x86_64-linux.template-dhall-diff
	nix build -L .#checks.x86_64-linux.template-version
	nix build -L .#checks.x86_64-linux.ctl-unit-test
	nix build -L .#checks.x86_64-linux.ctl-e2e-test
	nix build -L .#checks.x86_64-linux.ctl-plutip-test
	nix build -L .#checks.x86_64-linux.ctl-staking-test
	nix build -L .#checks.x86_64-linux.examples-imports-check

clean:
	@ rm -r .psc-ide-port || true
	@ rm -rf .psci_modules || true
	@ rm -rf .spago || true
	@ rm -rf generated-docs || true
	@ rm -rf .spago2nix || true
	@ rm -rf node_modules || true
	@ rm -rf output || true
	@ rm -rf dist || true
