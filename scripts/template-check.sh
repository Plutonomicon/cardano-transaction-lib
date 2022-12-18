#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]]; then
    set -o xtrace
fi

TEST_DIR="../ctl-template-test"

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./scripts/template-check.sh [REVISION]

This scripts initializes the ctl-scaffold template automatically and runs the
test suite in its nix shell. The provided revision will be used, or the
current git HEAD if none provided. This revision must be pushed upstream.
The script can be executed from outside of the development shell, but current
directory must be the project root.
The script assumes that CTL runtime is running.

This script will overwrite' "$TEST_DIR" 'directory.
'
    exit
fi

cd "$(dirname "$0")"

main() {
    if [[ -z "${1-}" ]]; then
        CTL_REVISION=""
        echo "CTL revision not provided, using current working directory"
    else
        CTL_REVISION="$1"
        echo "Using CTL revision $CTL_REVISION"
    fi;

    echo "This script assumes that CTL runtime is running."

    rm -rf "$TEST_DIR"
    mkdir -p "$TEST_DIR"
    cd "$TEST_DIR"
    if [[ -z "$CTL_REVISION" ]]; then
        nix flake init -t ../. # relative to `scripts/`
    else
        nix flake init -t github:Plutonomicon/cardano-transaction-lib/"$CTL_REVISION"
    fi;
    git init
    git config --local user.name 'test'
    git config --local user.email 'test@example.com'
    git add .
    git commit -m "Initial commit"
    nix develop -c npm run test
}

main "$@"
