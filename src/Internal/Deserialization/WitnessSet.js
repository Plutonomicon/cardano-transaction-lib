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

export var getVkeywitnesses = callMaybe("vkeys");
export {containerExtractor as extractWitnesses};
export var getVkey = call("vkey");
export var getSignature = call("signature");
export var vkeyPublicKey = call("public_key");
export var publicKeyToBech32 = call("to_bech32");
export var signatureToBech32 = call("to_bech32");
export var getNativeScripts = callMaybe("native_scripts");
export {containerExtractor as extractNativeScripts};

export function nativeScriptAs(maybe) {
  return prop => res => ns =>
    ns[prop]() == null ? maybe.nothing : maybe.just(res);
}

export var getBootstraps = callMaybe("bootstraps");
export {containerExtractor as extractBootstraps};
export var getBootstrapVkey = call("vkey");
export var getBootstrapSignature = call("signature");
export var getBootstrapChainCode = call("chain_code");
export var getBootstrapAttributes = call("attributes");
export var getPlutusScripts = callMaybe("plutus_scripts");
export {containerExtractor as extractPlutusScripts};
export var plutusScriptBytes = call("bytes");
export var plutusScriptVersion = call("language_version");
export var getWitnessSetPlutusData = callMaybe("plutus_data");
export {containerExtractor as extractPlutusData};
export var getRedeemers = callMaybe("redeemers");
export var getRedeemerTag = call("tag");
export var getRedeemerIndex = call("index");
export {containerExtractor as extractRedeemers};
export var getRedeemerTagKind = call("kind");
export var getRedeemerPlutusData = call("data");
export var getExUnits = call("ex_units");
export var getExUnitsMem = call("mem");
export var getExUnitsSteps = call("steps");
