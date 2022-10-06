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

exports.getVkeywitnesses = callMaybe("vkeys");
exports.extractWitnesses = containerExtractor;
exports.getVkey = call("vkey");
exports.getSignature = call("signature");
exports.vkeyPublicKey = call("public_key");
exports.publicKeyToBech32 = call("to_bech32");
exports.signatureToBech32 = call("to_bech32");
exports.getNativeScripts = callMaybe("native_scripts");
exports.extractNativeScripts = containerExtractor;
exports.nativeScriptAs = maybe => prop => res => ns =>
  ns[prop]() == null ? maybe.nothing : maybe.just(res);
exports.getBootstraps = callMaybe("bootstraps");
exports.extractBootstraps = containerExtractor;
exports.getBootstrapVkey = call("vkey");
exports.getBootstrapSignature = call("signature");
exports.getBootstrapChainCode = call("chain_code");
exports.getBootstrapAttributes = call("attributes");
exports.getPlutusScripts = callMaybe("plutus_scripts");
exports.extractPlutusScripts = containerExtractor;
exports.plutusScriptBytes = call("bytes");
exports.plutusScriptVersion = call("language_version");
exports.getWitnessSetPlutusData = callMaybe("plutus_data");
exports.extractPlutusData = containerExtractor;
exports.getRedeemers = callMaybe("redeemers");
exports.getRedeemerTag = call("tag");
exports.getRedeemerIndex = call("index");
exports.extractRedeemers = containerExtractor;
exports.getRedeemerTagKind = call("kind");
exports.getRedeemerPlutusData = call("data");
exports.getExUnits = call("ex_units");
exports.getExUnitsMem = call("mem");
exports.getExUnitsSteps = call("steps");
exports._deserializeWitnessSet = maybe => bytes => {
  try {
    return maybe.just(lib.TransactionWitnessSet.from_bytes(bytes));
  } catch (_) {
    return maybe.nothing;
  }
};
