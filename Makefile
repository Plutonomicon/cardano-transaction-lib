SHELL := bash
.ONESHELL:
.PHONY: run-dev run-build check-format format run-datum-cache-postgres-console query-testnet-tip clean
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd -epurs)
nix-sources := $(shell fd -enix --exclude='spago*')
hs-sources := $(shell fd . './server/src' './server/exe' -ehs)
ps-entrypoint := Examples.Pkh2Pkh
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js
node-ipc = $(shell docker volume inspect cardano-transaction-lib_node-ipc | jq -r '.[0].Mountpoint')
e2e-temp-dir := $(shell mktemp -tdu e2e.XXXXXXX)
# bump version here
e2e-test-chrome-dir := test-data/chrome-user-data
e2e-test-nami := test-data/chrome-extensions/nami_3.2.5_1.crx
e2e-test-nami-settings := test-data/nami_settings.tar.gz

# https://stackoverflow.com/questions/2214575/passing-arguments-to-make-run
# example: make e2e-test -- --no-headless
#
# If the first argument is "e2e-test,"...
ifeq (e2e-test,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "e2e-test"
  TEST_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(TEST_ARGS):;@:)
endif

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

check-format:
	@purs-tidy check ${ps-sources}
	nixpkgs-fmt --check ${nix-sources}
	fourmolu -m check -o -XTypeApplications -o -XImportQualifiedPost ${hs-sources}

e2e-test:
	@mkdir ${e2e-temp-dir}
	@unzip ${e2e-test-nami} -d ${e2e-temp-dir} > /dev/zero \
            || echo "ignore warnings" # or make stops
	@tar xzf ${e2e-test-nami-settings}
	rm -f test-data/chrome-user-data/SingletonLock
	@spago test --main Test.E2E -a "E2ETest --nami-dir=${e2e-temp-dir} $(TEST_ARGS)"

# extract current nami settings from e2e-test-chrome-dir and store them for git
nami-settings:
	tar czf ${e2e-test-nami-settings} ${e2e-test-chrome-dir}/Default/Local\ Extension\ Settings/lpfcbjknijpeeillifnkikgncikgfhdo/*

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
