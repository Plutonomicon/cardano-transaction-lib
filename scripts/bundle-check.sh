#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

if [[ "${TRACE-0}" == "1" ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./scripts/bundle-check.sh
Runs bundler commands and executes the tests. Make sure to start clt-runtime.
'
    exit
fi

# PS module to bundle & run via node
node_entry=Test.Ctl.Main
# PS module to bundle for the browser
browser_entry=Ctl.Examples.ByUrl

export PS_ENTRY_POINT
export BROWSER_RUNTIME
export PS_ENTRY_POINT_FUNCTION
export PS_ENTRY_POINT

PS_ENTRY_POINT_FUNCTION=main

function print_environment () {
    echo "BROWSER_RUNTIME=$BROWSER_RUNTIME bundler=$bundler PS_ENTRY_POINT_FUNCTION=$PS_ENTRY_POINT_FUNCTION PS_ENTRY_POINT=$PS_ENTRY_POINT"
}

for BROWSER_RUNTIME in "" "1"
do
    for bundler in "esbuild" "webpack"
    do
        if [ -z "$BROWSER_RUNTIME" ]; then
            PS_ENTRY_POINT=$node_entry
            for PS_ENTRY_POINT_FUNCTION in "main" ""
            do
                make $bundler-bundle
                print_environment
                if [ -z $PS_ENTRY_POINT_FUNCTION ]; then
                    node -e 'import("./dist/index.js").then(x => x.main())'
                else
                    node ./dist/index.js
                fi
            done
        else
            PS_ENTRY_POINT=$browser_entry
            print_environment
            make $bundler-bundle
            # e2e test suite can be used to check bundle validity
            # more carefully
        fi
    done
done
