SHELL := bash
.ONESHELL:
.PHONY: esbuild-bundle esbuild-serve webpack-bundle webpack-serve check-format format query-testnet-tip clean check-explicit-exports spago-build create-bundle-entrypoint create-html-entrypoint delete-bundle-entrypoint
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
js-sources := $(shell fd --no-ignore-parent -ejs -ecjs)

### Bundler setup

# The main Purescript module
ps-entrypoint := Scaffold.Test.E2E.Serve
# The entry point function in the main PureScript module
ps-entrypoint-function := main
# Whether to bundle for the browser ("1") or the node ("")
# NOTE: bundling for the node is not necessary, see https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/using-from-js.md
browser-runtime := 1 # Use "1" for true and "" for false

preview-node-ipc = $(shell docker volume inspect store_node-preview-ipc | jq -r '.[0].Mountpoint')
preprod-node-ipc = $(shell docker volume inspect store_node-preprod-ipc | jq -r '.[0].Mountpoint')
serve-port := 4008

spago-build:
	@spago build

create-bundle-entrypoint:
	@mkdir -p dist/
	@echo 'import("../output/${ps-entrypoint}/index.js").then(m => m.${ps-entrypoint-function}());' > ./dist/entrypoint.js

create-html-entrypoint:
	@mkdir -p dist/
	@cat << EOF > dist/index.html
	<!DOCTYPE html>
	<html>
	  <body><script type="module" src="./index.js"></script></body>
	</html>
	EOF

delete-bundle-entrypoint:
	@rm -f ./dist/entrypoint.js

esbuild-bundle: spago-build create-bundle-entrypoint
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

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

check-format:
	@purs-tidy check ${ps-sources}
	@prettier --loglevel warn -c ${js-sources}
	@eslint --quiet ${js-sources} --parser-options 'sourceType: module'

format:
	@purs-tidy format-in-place ${ps-sources}
	prettier -w ${js-sources}

clean:
	@ rm -r .psc-ide-port || true
	@ rm -rf .psci_modules || true
	@ rm -rf .spago || true
	@ rm -rf generated-docs || true
	@ rm -rf .spago2nix || true
	@ rm -rf node_modules || true
	@ rm -rf output || true
	@ rm -rf dist || true
