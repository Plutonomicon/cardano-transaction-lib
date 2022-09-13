#!/usr/bin/env bash

load_settings () {
    source "$(dirname "$(realpath "$0")")/e2e_defaults.env"
}

load_settings

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

extract_settings() {
    if [ -n "$1" ] && [ -f "$1" ]
    then
        tar xzf "$1"
    fi
}

run_tests () {
    local temp_dir="$(unique_temp_dir)"
    mkdir -p "$temp_dir"
    unzip "$NAMI_CRX" -d "$temp_dir"/nami > /dev/zero
    extract_settings "$NAMI_SETTINGS"
    unzip "$GERO_CRX" -d "$temp_dir"/gero > /dev/zero
    extract_settings "$GERO_SETTINGS"
    unzip "$FLINT_CRX" -d "$temp_dir"/flint > /dev/zero
    extract_settings "$FLINT_SETTINGS"
    unzip "$LODE_CRX" -d "$temp_dir"/lode > /dev/zero
    extract_settings "$LODE_SETTINGS"
    unzip "$ETERNL_CRX" -d "$temp_dir"/eternl > /dev/zero
    extract_settings "$ETERNL_SETTINGS"
    rm -f "$CHROME_PROFILE"/SingletonLock
    spago test --main Test.E2E -a "E2ETest \
          --eternl-dir $temp_dir/eternl \
          --eternl-password $ETERNL_PASSWORD \
          $* --chrome-exe $(find_browser)" || rm -Rf "$temp_dir"
}

extract_crx() {
    if [ -n "$1" ] && [ -f "$1" ]
    then
        unzip "$1" -d "$2" > /dev/zero
    fi
}


run_browser () {
    local temp_dir="$(unique_temp_dir)"
    echo TMP: $temp_dir
    mkdir -p "$temp_dir"

    extract_settings "$FLINT_SETTINGS"
    extract_settings "$GERO_SETTINGS"
    extract_settings "$LODE_SETTINGS"
    extract_settings "$NAMI_SETTINGS"
    extract_settings "$ETERNL_SETTINGS"
    extract_crx "$FLINT_CRX" "$temp_dir/flint"
    extract_crx "$GERO_CRX" "$temp_dir/gero"
    extract_crx "$LODE_CRX" "$temp_dir/lode"
    extract_crx "$NAMI_CRX" "$temp_dir/nami"
    extract_crx "$ETERNL_CRX" "$temp_dir/eternl"

    "$(find_browser)" \
        --load-extension="$temp_dir"/flint,"$temp_dir"/gero,"$temp_dir"/nami,"$temp_dir"/lode,"$temp_dir"/eternl \
        --user-data-dir="$CHROME_PROFILE" || rm -Rf "$temp_dir"

}

# TODO: rename to pack_settings
extract_settings_nami() {
    local extid="$1"
    local tgt="$2"

    tar czf "$tgt" "$CHROME_PROFILE"/Default/Local\ Extension\ Settings/"$extid"/*
}

extract_settings_gero_flint() {
    local extid="$1"
    local tgt="$2"

    tar czf "$tgt" \
        "$CHROME_PROFILE"/Default/IndexedDB/chrome-extension_"$extid"_0.indexeddb.leveldb \
        "$CHROME_PROFILE"/Default/Extension\ State
}

extract_settings_eternl() {
    local extid="$1"
    local tgt="$2"

    tar czf "$tgt" \
        "$CHROME_PROFILE"/Default/IndexedDB/chrome-extension_"$extid"_0.indexeddb.leveldb \
        "$CHROME_PROFILE"/Default/Extension\ State
}

extract_settings_lode() {
    local extid="$1"
    local tgt="$2"

    tar czf "$tgt" \
        "$CHROME_PROFILE"/Default/Local\ Extension\ Settings/"$extid"/* \
        "$CHROME_PROFILE"/Default/Extension\ State
}

usage() {
    echo "Usage: $0 cmd [options]"
    echo " commands:"
    echo "   run [--no-headless]  Run the tests"
    echo "   browser              Start the browser in order to configure wallets"
    echo "   settings [nami|gero|flint|lode] [-o out] "
    echo "                        Dump wallet setting to out"
    exit 1
}

cmd_settings() {
    case "$1" in
        "flint")
            CMD=extract_settings_gero_flint
            EXTID="$FLINT_EXTID"
            TARGET="$FLINT_SETTINGS"
            ;;
        "eternl")
            CMD=extract_settings_eternl
            EXTID="$ETERNL_EXTID"
            TARGET="$ETERNL_SETTINGS"
            ;;
        "gero")
            CMD=extract_settings_gero_flint
            EXTID="$GERO_EXTID"
            TARGET="$GERO_SETTINGS"
            ;;
        "nami")
            CMD=extract_settings_nami
            EXTID="$NAMI_EXTID"
            TARGET="$NAMI_SETTINGS"
            ;;
        "lode")
            CMD=extract_settings_lode
            EXTID="$LODE_EXTID"
            TARGET="$LODE_SETTINGS"
            ;;
        *)
            usage
            ;;
    esac

    shift 1
    while getopts "o:" arg; do
        case "$arg" in
            o)
                TARGET="$OPTARG"
                ;;
        esac
    done

    "$CMD" "$EXTID" "$TARGET"
}

if [ -z "$1" ]
then
    usage
fi

case "$1" in
    "run")
        shift 1
        run_tests $*
        ;;
    "browser")
        shift 1
        run_browser
        ;;
    "settings")
        shift 1
        cmd_settings $*
        ;;
    *)
        usage
        ;;
esac
