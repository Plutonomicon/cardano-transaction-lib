/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
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

export const getInput = call("input");
export const getOutput = call("output");
export const getTransactionHash = call("transaction_id");
export const getTransactionIndex = call("index");
export const getAddress = call("address");
export const getPlutusData = callMaybe("plutus_data");
export const getScriptRef = callMaybe("script_ref");

export function withScriptRef(ccNativeScript) {
  return ccPlutusScript => scriptRef => {
    if (scriptRef.is_native_script()) {
      return ccNativeScript(scriptRef.native_script());
    } else if (scriptRef.is_plutus_script()) {
      return ccPlutusScript(scriptRef.plutus_script());
    } else {
      throw "Impossible happened: withScriptRef: not a script";
    }
  };
}

export const getAmount = call("amount");
export const getCoin = call("coin");
export const getMultiAsset = callMaybe("multiasset");
export { extractDict as extractMultiAsset };
export { extractDict as extractAssets };
export const getDataHash = callMaybe("data_hash");

export function mkTransactionUnspentOutput(input) {
  return output => lib.TransactionUnspentOutput.new(input, output);
}
