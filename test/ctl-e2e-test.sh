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
    BROWSER=$(find_browser)
    if [[ "$BROWSER" == *"snap"* ]]; then
	echo ./tmp
    else
	eche $TMPDIR
    fi
}

unique_temp_dir () {
    echo "$(temp_base)/$(mktemp -du e2e.XXXXXXX)"
} 

run_tests () {
    UNIQUE_TEMP_DIR=$(unique_temp_dir)
    mkdir -p $UNIQUE_TEMP_DIR
    unzip $NAMI_CRX -d $UNIQUE_TEMP_DIR/nami > /dev/zero
    tar xzf $NAMI_SETTINGS
    unzip $GERO_CRX -d $UNIQUE_TEMP_DIR/gero > /dev/zero
    tar xzf $GERO_SETTINGS
    unzip $FLINT_CRX -d $UNIQUE_TEMP_DIR/flint > /dev/zero
    tar xzf $FLINT_SETTINGS
    unzip $LODE_CRX -d $UNIQUE_TEMP_DIR/lode > /dev/zero
    tar xzf $LODE_SETTINGS
    rm -f $CHROME_PROFILE/SingletonLock
    spago test --main Test.E2E -a "E2ETest \
    	  --nami-dir $UNIQUE_TEMP_DIR/nami \
	  --nami-password $NAMI_PASSWORD \
	  --gero-dir $UNIQUE_TEMP_DIR/gero \
	  --gero-password $GERO_PASSWORD \
	  --lode-dir $UNIQUE_TEMP_DIR/lode \
 	  --lode-password $LODE_PASSWORD \
 	  --flint-dir $UNIQUE_TEMP_DIR/flint \
 	  --flint-password $FLINT_PASSWORD \
	  $* --chrome-exe $(find_browser)" || rm -Rf $UNIQUE_TEMP_DIR
}

extract_settings() {
    if [ -n "$1" ] && [ -f "$1" ]
    then
	tar xzf "$1"
    fi
}

extract_crx() {
    if [ -n "$1" ] && [ -f "$1" ]
    then
	unzip $1 -d $2 > /dev/zero
    fi
}


run_browser () {
    UNIQUE_TEMP_DIR=$(unique_temp_dir)    
    mkdir -p $UNIQUE_TEMP_DIR

    extract_settings "$FLINT_SETTINGS"
    extract_settings "$GERO_SETTINGS"
    extract_settings "$LODE_SETTINGS"    
    extract_settings "$NAMI_SETTINGS"
    extract_crx "$FLINT_CRX" "$UNIQUE_TEMP_DIR/flint"
    extract_crx "$GERO_CRX" "$UNIQUE_TEMP_DIR/gero"
    extract_crx "$LODE_CRX" "$UNIQUE_TEMP_DIR/lode"    
    extract_crx "$NAMI_CRX" "$UNIQUE_TEMP_DIR/nami"

    $(find_browser) \
	--load-extension=$UNIQUE_TEMP_DIR/flint,$UNIQUE_TEMP_DIR/gero,$UNIQUE_TEMP_DIR/nami \
	--user-data-dir=$CHROME_PROFILE || rm -Rf $UNIQUE_TEMP_DIR
    
}

EXTID_NAMI=lpfcbjknijpeeillifnkikgncikgfhdo
EXTID_GERO=iifeegfcfhlhhnilhfoeihllenamcfgc
EXTID_FLINT=hnhobjmcibchnmglfbldbfabcgaknlkj
EXTID_LODE=ikffplhknjhbfkgbhnionfklokakmknd

extract_settings_nami() {
    extid=$1    
    tgt=$2

    tar czf $tgt $CHROME_PROFILE/Default/Local\ Extension\ Settings/$extid/*
}

extract_settings_gero_flint() {
    extid=$1    
    tgt=$2

    tar czf $tgt \
	$CHROME_PROFILE/Default/IndexedDB/chrome-extension_${extid}_0.indexeddb.leveldb \
	$CHROME_PROFILE/Default/Extension\ State
}


MODE=""


usage() {
    echo "Usage: $0 cmd [options]"
    echo " commands:"
    echo "   run                  Run the tests"
    echo "   browser              Start the browser in order to configure wallets"
    echo "   settings [nami|gero|flint|lode] [-o out] "
    echo "                        Dump wallet setting to out"
    exit 1
}

cmd_run() {
    run_tests
}

cmd_browser() {
    run_browser
}

cmd_settings() {
    case "$1" in
	"flint")
	    CMD=extract_settings_gero_flint
	    EXTID=$EXTID_FLINT
	    TARGET=$FLINT_SETTINGS
	    ;;
	"gero")
	    CMD=extract_settings_gero_flint
	    EXTID=$EXTID_GERO
	    TARGET=$GERO_SETTINGS
	    ;;
	"nami")
	    CMD=extract_settings_nami
	    EXTID=$EXTID_NAMI
	    TARGET=$NAMI_SETTINGS
	    ;;	
	"lode")
	    CMD=extract_settings_gero_flint
	    EXTID=$EXTID_LODE
	    TARGET=$LODE_SETTINGS
	    ;;	
    esac

    shift 1   
    while getopts "o:" arg; do
	case $arg in
	    o)
		TARGET="$OPTARG"
		;;
	esac
    done
    
    $CMD $EXTID $TARGET    
}

if [ -z "$1" ]
then
    usage
fi

case "$1" in
    "run") 
	shift 1
	cmd_run $*
	;;
    "browser") 
	shift 1
	cmd_browser $*
	;;    
    "settings") 
	shift 1
	cmd_settings $*
	;;    
    "*")
	usage
	;;
esac
