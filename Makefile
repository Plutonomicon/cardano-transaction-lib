SHELL := bash
.ONESHELL:
.PHONY: run-dev run-build check-format format run-datum-cache-postgres-console
		query-testnet-tip clean check-explicit-exports e2e-test
.SHELLFLAGS := -eu -o pipefail -c

# find a suitable browser for e2e-tests
define e2e-browser
	$(shell which chromium google-chrome 2>/dev/null | head -n1)
endef

# find a suitable temp directory for e2e-tests. snaps don't work
# in $TMPDIR, because of lacking read access!
define e2e-temp-base
	$(if $(findstring snap,$(call e2e-browser)),./tmp,$TMPDIR)
endef

ps-sources := $(shell fd -epurs -Etmp)
nix-sources := $(shell fd -enix --exclude='spago*' -Etmp)
hs-sources := $(shell fd . './server/src' './server/exe' -ehs -Etmp)
js-sources := $(shell fd -ejs -Etmp)
ps-entrypoint := Examples.ByUrl
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js
node-ipc = $(shell docker volume inspect cardano-transaction-lib_node-ipc | jq -r '.[0].Mountpoint')
e2e-temp-dir := $(strip $(call e2e-temp-base))/$(shell mktemp -du e2e.XXXXXXX)
e2e-test-chrome-dir := test-data/chrome-user-data

# bump version here
e2e-test-nami := test-data/chrome-extensions/nami_3.2.5_1.crx
e2e-test-nami-settings := test-data/nami_settings.tar.gz

# bump version here
e2e-test-gero := test-data/chrome-extensions/gero_testnet_1.10.0_0.crx
e2e-test-gero-settings := test-data/gero_settings.tar.gz

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

.ONESHELL:
check-explicit-exports:
	@if grep -rn '(\.\.)' ./src ./test ./examples; then
		echo "Use explicit imports/exports ^"
		exit 1
	else
		echo "All imports/exports are explicit"
	fi

check-format: check-explicit-exports
	@purs-tidy check ${ps-sources}
	@nixpkgs-fmt --check ${nix-sources}
	@fourmolu -m check -o -XTypeApplications -o -XImportQualifiedPost ${hs-sources}
	@prettier -c ${js-sources}
	@eslint --quiet ${js-sources}

e2e-test:
	@mkdir -p ${e2e-temp-dir}
	@unzip ${e2e-test-nami} -d ${e2e-temp-dir}/nami > /dev/zero \
	    || echo "ignore warnings" # or make stops
	@tar xzf ${e2e-test-nami-settings}
	@unzip ${e2e-test-gero} -d ${e2e-temp-dir}/gero > /dev/zero \
	    || echo "ignore warnings" # or make stops
	@tar xzf ${e2e-test-gero-settings}
	@rm -f ${e2e-test-chrome-dir}/SingletonLock
	@spago test --main Test.E2E -a "E2ETest --nami-dir ${e2e-temp-dir}/nami --gero-dir ${e2e-temp-dir}/gero $(TEST_ARGS) --chrome-exe $(call e2e-browser)" || rm -Rf ${e2e-temp-dir}

e2e-run-browser-nami:
	@mkdir -p ${e2e-temp-dir}
	@unzip ${e2e-test-nami} -d ${e2e-temp-dir}/nami > /dev/zero \
	    || echo "ignore warnings" # or make stops
	@tar xzf ${e2e-test-nami-settings}
	@$(call e2e-browser) --load-extension=${e2e-temp-dir}/nami --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e-temp-dir}

e2e-run-browser-gero:
	@mkdir -p ${e2e-temp-dir}
	@unzip ${e2e-test-gero} -d ${e2e-temp-dir}/gero > /dev/zero \
	   || echo "ignore warnings" # or make stops
	@tar xzf ${e2e-test-gero-settings}
	echo $(call e2e-browser) --load-extension=${e2e-temp-dir}/gero --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e-temp-dir}
	$(call e2e-browser) --load-extension=${e2e-temp-dir}/gero --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e-temp-dir}

# extract current nami settings from e2e-test-chrome-dir and store them for git
nami-settings:
	tar czf ${e2e-test-nami-settings} ${e2e-test-chrome-dir}/Default/Local\ Extension\ Settings/lpfcbjknijpeeillifnkikgncikgfhdo/*

# extract current gero settings from e2e-test-chrome-dir and store them for git
gero-settings:
	tar czf ${e2e-test-gero-settings} \
		${e2e-test-chrome-dir}/Default/IndexedDB/chrome-extension_iifeegfcfhlhhnilhfoeihllenamcfgc_0.indexeddb.leveldb \
		${e2e-test-chrome-dir}/Default/Extension\ State

format:
	@purs-tidy format-in-place ${ps-sources}
	nixpkgs-fmt ${nix-sources}
	fourmolu -m inplace -o -XTypeApplications -o -XImportQualifiedPost ${hs-sources}
	prettier -w ${js-sources}

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

tgt:
	@echo $(call e2e-temp-base,$(call e2e-browser))
