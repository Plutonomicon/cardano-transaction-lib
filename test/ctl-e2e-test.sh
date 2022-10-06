#!/usr/bin/env bash

# Load the environment
source "$(dirname "$(realpath "$0")")/e2e_defaults.env"

# find a suitable browser for e2e-tests
find_browser () {
    which chromium google-chrome | head -n1
}

# find a suitable temp directory for e2e-tests. snaps don't work
# in $TMPDIR, because of lacking read access!
temp_base () {
    local browser="$(find_browser)"
    if [[ "$browser" == *"snap"* ]]; then
        echo ./tmp
    else
        if [[ -z "$TMPDIR" ]]; then
            echo ./tmp
        else
            echo "$TMPDIR"
        fi
    fi
}

unique_temp_dir () {
    echo "$(temp_base)/$(mktemp -du e2e.XXXXXXX)"
}

extract_crx() {
    if [ -n "$1" ] && [ -f "$1" ]
    then
        unzip "$1" -d "$2" > /dev/zero
    fi
}


pack_settings () {
    tar czf "$SETTINGS_ARCHIVE" \
        "$CHROME_PROFILE"/Default/IndexedDB/ \
        "$CHROME_PROFILE"/Default/Local\ Storage/ \
        "$CHROME_PROFILE"/Default/Extension\ State \
        "$CHROME_PROFILE"/Default/Local\ Extension\ Settings
}

unpack_settings () {
    tar xzf "$SETTINGS_ARCHIVE"
}

run_tests () {
    local temp_dir="$(unique_temp_dir)"
    mkdir -p "$temp_dir"

    unpack_settings "$SETTINGS_ARCHIVE"
    extract_crx "$NAMI_CRX" "$temp_dir"/nami
    extract_crx "$GERO_CRX" "$temp_dir"/gero
    extract_crx "$FLINT_CRX" "$temp_dir"/flint
    extract_crx "$LODE_CRX" "$temp_dir"/lode
    extract_crx "$ETERNL_CRX" "$temp_dir"/eternl
    rm -f "$CHROME_PROFILE"/SingletonLock
    spago test --main Test.Ctl.E2E -a "E2ETest \
          --eternl-dir $temp_dir/eternl \
          --eternl-password $ETERNL_PASSWORD \
          --lode-dir $temp_dir/lode \
          --lode-password $LODE_PASSWORD \
          $* --chrome-exe $(find_browser)" || rm -Rf "$temp_dir"
}

run_browser () {
    local temp_dir="$(unique_temp_dir)"
    echo TMP: $temp_dir
    mkdir -p "$temp_dir"

    unpack_settings "$SETTINGS_ARCHIVE"

    extract_crx "$FLINT_CRX" "$temp_dir/flint"
    extract_crx "$GERO_CRX" "$temp_dir/gero"
    extract_crx "$LODE_CRX" "$temp_dir/lode"
    extract_crx "$NAMI_CRX" "$temp_dir/nami"
    extract_crx "$ETERNL_CRX" "$temp_dir/eternl"

    "$(find_browser)" \
        --load-extension="$temp_dir"/flint,"$temp_dir"/gero,"$temp_dir"/nami,"$temp_dir"/lode,"$temp_dir"/eternl \
        --user-data-dir="$CHROME_PROFILE" || rm -Rf "$temp_dir"

}

usage() {
    echo "Usage: $0 cmd [options]"
    echo " commands:"
    echo "   run [-s SETTINGS] [-- --no-headless] Run the tests"
    echo "   browser [-s SETTINGS]                Start the browser in order to configure wallets"
    echo "   pack [-s SETTINGS]                   Save wallet setting to 'out'"
    echo "   unpack [-s SETTINGS]                 Read wallet setting from 'in'"
    exit 1
}

if [ -z "$1" ]
then
    usage
fi

CMD="$1"

SETTINGS_ARCHIVE="test-data/settings.tar.gz"

while getopts "s:" arg; do
    case "$arg" in
        s)
            SETTINGS_ARCHIVE="$OPTARG"
            ;;
    esac
done

case "$CMD" in
    "run")
        shift 1
        run_tests $*
        ;;
    "browser")
        run_browser
        ;;
    "pack")
        pack_settings
        ;;
    "unpack")
        unpack_settings
        ;;
    *)
        usage
        ;;
esac
