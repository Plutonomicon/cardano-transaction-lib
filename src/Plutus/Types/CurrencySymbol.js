/* global require exports BROWSER_RUNTIME */

var CardanoWasm;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    CardanoWasm = require('@ngua/cardano-serialization-lib-browser');
} else {
    CardanoWasm = require('@ngua/cardano-serialization-lib-nodejs');
}

exports._unsafeScriptHashFromBytes = input => {
    return CardanoWasm.ScriptHash.from_bytes(input);
};

