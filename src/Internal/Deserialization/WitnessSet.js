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

export const getVkeywitnesses = callMaybe("vkeys");
export { containerExtractor as extractWitnesses };
export const getVkey = call("vkey");
export const getSignature = call("signature");
export const vkeyPublicKey = call("public_key");
export const publicKeyToBech32 = call("to_bech32");
export const signatureToBech32 = call("to_bech32");
export const getNativeScripts = callMaybe("native_scripts");
export { containerExtractor as extractNativeScripts };

export function nativeScriptAs(maybe) {
  return prop => res => ns =>
    ns[prop]() == null ? maybe.nothing : maybe.just(res);
}

export const getBootstraps = callMaybe("bootstraps");
export { containerExtractor as extractBootstraps };
export const getBootstrapVkey = call("vkey");
export const getBootstrapSignature = call("signature");
export const getBootstrapChainCode = call("chain_code");
export const getBootstrapAttributes = call("attributes");
export const getPlutusScripts = callMaybe("plutus_scripts");
export { containerExtractor as extractPlutusScripts };
export const plutusScriptBytes = call("bytes");
export const plutusScriptVersion = call("language_version");
export const getWitnessSetPlutusData = callMaybe("plutus_data");
export { containerExtractor as extractPlutusData };
export const getRedeemers = callMaybe("redeemers");
export const getRedeemerTag = call("tag");
export const getRedeemerIndex = call("index");
export { containerExtractor as extractRedeemers };
export const getRedeemerTagKind = call("kind");
export const getRedeemerPlutusData = call("data");
export const getExUnits = call("ex_units");
export const getExUnitsMem = call("mem");
export const getExUnitsSteps = call("steps");
