SHELL ?= bash
.ONESHELL:
.PHONY: esbuild-bundle esbuild-serve webpack-bundle webpack-serve check-format format query-testnet-tip clean check-explicit-exports spago-build create-bundle-entrypoint create-html-entrypoint delete-bundle-entrypoint
.SHELLFLAGS ?= -eu -o pipefail -c

PS_SOURCES ?= $(shell fd --no-ignore-parent -epurs)
NIX_SOURCES ?= $(shell fd --no-ignore-parent -enix --exclude='spago*')
JS_SOURCES ?= $(shell fd --no-ignore-parent -ejs -ecjs)

### Bundler setup

# The main Purescript module
PS_ENTRY_POINT ?= Ctl.Examples.ByUrl
# The entry point function in the main PureScript module
# Leave empty to bundle as API module and not as executable script.
PS_ENTRY_POINT_FUNCTION ?= main
# Whether to bundle for the browser
BROWSER_RUNTIME ?= 1 # Use "1" for true and "" for false

PREVIEW_NODE_IPC = $(shell docker volume inspect store_node-preview-ipc | jq -r '.[0].Mountpoint')
PREPROD_NODE_IPC = $(shell docker volume inspect store_node-preprod-ipc | jq -r '.[0].Mountpoint')
SERVE_PORT ?= 4008

spago-build:
	@spago build

create-bundle-entrypoint:
	@mkdir -p dist/
ifdef PS_ENTRY_POINT_FUNCTION
	  @echo 'import("../output/${PS_ENTRY_POINT}/index.js").then(m => m.${PS_ENTRY_POINT_FUNCTION}());' > ./dist/entrypoint.js
endif

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
ifdef PS_ENTRY_POINT_FUNCTION
	node esbuild/bundle.js \
		./dist/entrypoint.js \
		dist/index.js
else
	node esbuild/bundle.js \
		./output/${PS_ENTRY_POINT}/index.js \
		dist/index.js
endif
	@make delete-bundle-entrypoint

esbuild-serve: spago-build create-bundle-entrypoint create-html-entrypoint
ifdef PS_ENTRY_POINT_FUNCTION
	  BROWSER_RUNTIME=1 node esbuild/serve.js ./dist/entrypoint.js dist/index.js dist/ ${SERVE_PORT}
else
	  @echo "'PS_ENTRY_POINT_FUNCTION' Makefile variable must be set for 'esbuild-serve'"
	  @echo "Serving an API library does not make sense."
	  exit 1
endif

webpack-bundle: spago-build create-bundle-entrypoint
ifdef PS_ENTRY_POINT_FUNCTION
	webpack --mode=production \
		-o dist/ --env entry=./dist/entrypoint.js
	@make delete-bundle-entrypoint
else
	webpack --mode=production \
		-o dist/ --env entry="./output/${PS_ENTRY_POINT}/index.js"
endif

webpack-serve: spago-build create-bundle-entrypoint create-html-entrypoint
ifdef PS_ENTRY_POINT_FUNCTION
	BROWSER_RUNTIME=1 webpack-dev-server --progress \
		--port ${SERVE_PORT} \
		-o dist/ --env entry=./dist/entrypoint.js
else
	@echo "'PS_ENTRY_POINT_FUNCTION' Makefile variable must be set for 'webpack-serve'"
	@echo "Serving an API library does not make sense."
	exit 1
endif

.ONESHELL:
check-explicit-exports:
	@if grep -rn '(\.\.)' ${PS_SOURCES}; then
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
	@purs-tidy check ${PS_SOURCES}
	@nixpkgs-fmt --check ${NIX_SOURCES}
	@prettier --loglevel warn -c ${JS_SOURCES}
	@eslint --quiet ${JS_SOURCES} --parser-options 'sourceType: module'

format:
	@purs-tidy format-in-place ${PS_SOURCES}
	nixpkgs-fmt ${NIX_SOURCES}
	prettier -w ${JS_SOURCES}
	doctoc CHANGELOG.md README.md doc/*.md --github --notitle
	make check-explicit-exports
	make check-examples-imports
	make check-whitespace

query-preview-testnet-tip:
	CARDANO_NODE_SOCKET_PATH=${PREVIEW_NODE_IPC}/node.socket cardano-cli query tip \
	  --testnet-magic 2

query-preprod-testnet-tip:
	CARDANO_NODE_SOCKET_PATH=${PREPROD_NODE_IPC}/node.socket cardano-cli query tip \
	  --testnet-magic 1

run-ci-actions:
	nix build -L .#checks.x86_64-linux.formatting-check
	nix build -L .#checks.x86_64-linux.template-deps-json
	nix build -L .#checks.x86_64-linux.template-dhall-diff
	nix build -L .#checks.x86_64-linux.template-version
	nix build -L .#checks.x86_64-linux.ctl-unit-test
	nix build -L .#checks.x86_64-linux.ctl-e2e-test
	nix build -L .#checks.x86_64-linux.ctl-e2e-test-esbuild
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
