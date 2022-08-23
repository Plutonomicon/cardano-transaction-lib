#!/usr/bin/env bash

# find a suitable browser for e2e-tests
e2e_browser () {
    which chromium google-chrome | head -n1
}

# find a suitable temp directory for e2e-tests. snaps don't work
# in $TMPDIR, because of lacking read access!
e2e_temp_base () {
    BROWSER=$(e2e_browser)
    if [[ "$BROWSER" == *"snap"* ]]; then
	echo ./tmp
    else
	eche $TMPDIR
    fi
}

e2e_temp_dir () {
    echo "$(e2e_temp_base)/$(mktemp -du e2e.XXXXXXX)"
}

e2e_test_chrome_dir="test-data/chrome-user-data"

# bump version here
e2e_test_nami="test-data/chrome-extensions/nami_3.2.5_1.crx"
e2e_test_nami_settings="test-data/nami_settings.tar.gz"

# bump version here
e2e_test_flint="test-data/chrome-extensions/flint_1.16.2_0.crx"
e2e_test_flint_settings="test-data/flint_settings.tar.gz"

# bump version here
e2e_test_gero="test-data/chrome-extensions/gero_testnet_1.10.0_0.crx"
e2e_test_gero_settings="test-data/gero_settings.tar.gz"

e2e_test () {
	mkdir -p $(e2e_temp_dir)
	unzip $e2e_test_nami -d $(e2e_temp_dir)/nami > /dev/zero
	tar xzf $e2e_test_nami_settings
	unzip $e2e_test_gero -d $(e2e_temp_dir)/gero > /dev/zero
	tar xzf $e2e_test_gero_settings
	unzip $e2e_test_flint -d $(e2e_temp_dir)/gero > /dev/zero
	tar xzf $e2e_test_flint_settings
	rm -f $e2e_test_chrome_dir/SingletonLock
	spago test --main Test.E2E -a "E2ETest --nami-dir $(e2e_temp_dir)/nami --gero-dir $(e2e_temp_dir)/gero $* --chrome-exe $(e2e_browser)" || rm -Rf $(e2e_temp_dir)
}

#e2e-run-browser-nami:
#	@mkdir -p ${e2e_temp_dir}
#	@unzip ${e2e-test-nami} -d ${e2e_temp_dir}/nami > /dev/zero || true
#	@tar xzf ${e2e-test-nami-settings}
#	@$(call e2e-browser) --load-extension=${e2e_temp_dir}/nami --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e_temp_dir}
#
#e2e-run-browser-gero:
#	@mkdir -p ${e2e_temp_dir}
#	@unzip ${e2e-test-gero} -d ${e2e_temp_dir}/gero > /dev/zero || true
#	@tar xzf ${e2e-test-gero-settings}
#	echo $(call e2e-browser) --load-extension=${e2e_temp_dir}/gero --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e_temp_dir}
#	$(call e2e-browser) --load-extension=${e2e_temp_dir}/gero --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e_temp_dir}
#
#e2e-run-browser-flint:
#	@mkdir -p ${e2e_temp_dir}
#	@unzip ${e2e-test-flint} -d ${e2e_temp_dir}/flint > /dev/zero || true
#	@tar xzf ${e2e-test-flint-settings}
#	echo $(call e2e-browser) --load-extension=${e2e_temp_dir}/flint --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e_temp_dir}
#	$(call e2e-browser) --load-extension=${e2e_temp_dir}/flint --user-data-dir=${e2e-test-chrome-dir} || rm -Rf ${e2e_temp_dir}

# extract current nami settings from e2e-test-chrome-dir and store them for git
#nami-settings:
#	tar czf ${e2e-test-nami-settings} ${e2e-test-chrome-dir}/Default/Local\ Extension\ Se#ttings/lpfcbjknijpeeillifnkikgncikgfhdo/*

# extract current gero settings from e2e-test-chrome-dir and store them for git
#gero-settings:
#	tar czf ${e2e-test-gero-settings} \
#		${e2e-test-chrome-dir}/Default/IndexedDB/chrome-extension_iifeegfcfhlhhnilhfoeihllenamcfgc_0.indexeddb.leveldb \
#		${e2e-test-chrome-dir}/Default/Extension\ State
#
# extract current gero settings from e2e-test-chrome-dir and store them for git
#flint-settings:
#	tar czf ${e2e-test-flint-settings} \
#		${e2e-test-chrome-dir}/Default/IndexedDB/chrome-extension_hnhobjmcibchnmglfbldbfabcgaknlkj_0.indexeddb.leveldb \
#		${e2e-test-chrome-dir}/Default/Extension\
#
#tgt:
#	@echo $(call e2e_temp_base,$(call e2e-browser))

MODE=""


usage() {
    echo "Usage: $0 cmd"
    echo " commands:"
    echo "   run      run the tests"
}


cmd_run() {
    e2e_test
}

case "$1" in
    "run") 
	shift 1
	cmd_run $*
	;;
    "*")
	usage
	;;
esac
