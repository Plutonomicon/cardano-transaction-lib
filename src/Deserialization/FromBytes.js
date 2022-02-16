/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

const fromBytes = name => helper => bytes => {
    try {
        return helper.just(lib[name].from_bytes(bytes));
    } catch (e) {
        return helper.nothing;
    }
};

exports._fromBytesAddress = fromBytes('Address');
exports._fromBytesScriptHash = fromBytes('ScriptHash');
exports._fromBytesDataHash = fromBytes('DataHash');
exports._fromBytesEd25519KeyHash = fromBytes('Ed25519KeyHash');
exports._fromBytesTransactionHash = fromBytes('TransactionHash');
exports._fromBytesPlutusData = fromBytes('PlutusData');
exports._fromBytesTransactionUnspentOutput = fromBytes('TransactionUnspentOutput');
exports._fromBytesTransactionWitnessSet = fromBytes('TransactionWitnessSet');
exports._fromBytesNativeScript = fromBytes('NativeScript');
