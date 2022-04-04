/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

exports.attachSignatureLocally = txArray => witnessesArray => () => {
    const tx = lib.Transaction.from_bytes(txArray);
    const newWits = lib.TransactionWitnessSet.from_bytes(witnessesArray);
    const newvkeyWits = newWits.vkeys();
    const wits = tx.witness_set();
    wits.set_vkeys(newvkeyWits);
    return lib.Transaction.new(tx.body(), wits, tx.auxiliary_data()).to_bytes();
};
