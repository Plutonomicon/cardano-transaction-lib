SHELL := bash
.PHONY: esbuild-bundle esbuild-serve
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c


ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
js-sources := $(shell fd --no-ignore-parent -ejs)
# points to one of the example PureScript modules in examples/
ps-entrypoint := Scaffold.Test.E2E.Serve
ps-entrypoint-function := main
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

esbuild-bundle: spago-build create-bundle-entrypoint
	@mkdir -p dist/
	@BROWSER_RUNTIME=1 node esbuild/bundle.js ./dist/entrypoint.js dist/index.js

esbuild-serve: spago-build create-bundle-entrypoint create-html-entrypoint

	BROWSER_RUNTIME=1 node esbuild/serve.js ./dist/entrypoint.js dist/index.js dist/ ${serve-port}

webpack-bundle: spago-build create-bundle-entrypoint
	BROWSER_RUNTIME=1 webpack --mode=production \
		-o dist/ --env entry=./dist/entrypoint.js

webpack-serve: spago-build create-bundle-entrypoint create-html-entrypoint
	BROWSER_RUNTIME=1 webpack-dev-server --progress \
		--port ${serve-port} \
		-o dist/ --env entry=./dist/entrypoint.js

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

e2e-serve:
	spago bundle-module -m ${e2e-entrypoint} --to output.js
	BROWSER_RUNTIME=1 webpack-dev-server --progress

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
