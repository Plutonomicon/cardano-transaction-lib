/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const fromBytes = name => helper => bytes => {
  try {
    return helper.valid(lib[name].from_bytes(bytes));
  } catch (e) {
    return helper.error(name + ".from_bytes() raised " + e);
  }
};

exports._fromBytesDataHash = fromBytes("DataHash");
exports._fromBytesTransaction = fromBytes("Transaction");
exports._fromBytesTransactionHash = fromBytes("TransactionHash");
exports._fromBytesPlutusData = fromBytes("PlutusData");
exports._fromBytesTransactionUnspentOutput = fromBytes(
  "TransactionUnspentOutput"
);
exports._fromBytesTransactionWitnessSet = fromBytes("TransactionWitnessSet");
exports._fromBytesNativeScript = fromBytes("NativeScript");
exports._fromBytesMint = fromBytes("Mint");
exports._fromBytesVRFKeyHash = fromBytes("VRFKeyHash");
exports._fromBytesValue = fromBytes("Value");
