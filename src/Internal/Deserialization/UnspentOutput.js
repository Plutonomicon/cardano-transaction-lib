/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const call = property => object => object[property]();
const callMaybe = property => maybe => object => {
  const res = object[property]();
  return res != null ? maybe.just(res) : maybe.nothing;
};

// Classes like TransactionInputs are just monomorphic containers with `len`
// and `get()` methods. This function abstracts away converting them to Array
// of something.
const containerExtractor = obj => {
  const res = [];

  for (let i = 0; i < obj.len(); i++) {
    res.push(obj.get(i));
  }

  return res;
};

// Dictionary wrappers share the same interface (with functions keys() and
// get(k)).
const extractDict = tuple => dict => {
  const keys = containerExtractor(dict.keys());
  const res = [];
  for (let key of keys) {
    // We assume that something that is in keys() is always present in the
    // structure as well.
    res.push(tuple(key)(dict.get(key)));
  }
  return res;
};

exports.getInput = call("input");
exports.getOutput = call("output");
exports.getTransactionHash = call("transaction_id");
exports.getTransactionIndex = call("index");
exports.getAddress = call("address");
exports.getPlutusData = callMaybe("plutus_data");
exports.getScriptRef = callMaybe("script_ref");
exports.withScriptRef = ccNativeScript => ccPlutusScript => scriptRef => {
  if (scriptRef.is_native_script()) {
    return ccNativeScript(scriptRef.native_script());
  } else if (scriptRef.is_plutus_script()) {
    return ccPlutusScript(scriptRef.plutus_script());
  } else {
    throw "Impossible happened: withScriptRef: not a script";
  }
};

exports.getAmount = call("amount");
exports.getCoin = call("coin");
exports.getMultiAsset = callMaybe("multiasset");
exports.extractMultiAsset = extractDict;
exports.extractAssets = extractDict;
exports.getDataHash = callMaybe("data_hash");
exports.mkTransactionUnspentOutput = input => output =>
  lib.TransactionUnspentOutput.new(input, output);
exports._newTransactionUnspentOutputFromBytes = maybe => bytes => {
  try {
    return maybe.just(lib.TransactionUnspentOutput.from_bytes(bytes));
  } catch (_) {
    return maybe.nothing;
  }
};
