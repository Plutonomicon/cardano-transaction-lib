/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export function newTransactionWitnessSet() {
  return lib.TransactionWitnessSet.new();
}

export function newPublicKey(bech32) {
  return () => lib.PublicKey.from_bech32(bech32);
}

export function newVkeyFromPublicKey(public_key) {
  return () => lib.Vkey.new(public_key);
}

export function newVkeywitnesses() {
  return lib.Vkeywitnesses.new();
}

export function newVkeywitness(vkey) {
  return signature => () => lib.Vkeywitness.new(vkey, signature);
}

export function addVkeywitness(witnesses) {
  return witness => () => witnesses.add(witness);
}

export function newPlutusScripts() {
  return lib.PlutusScripts.new();
}

export function addPlutusScript(scripts) {
  return script => () => scripts.add(script);
}

export function transactionWitnessSetSetVkeys(ws) {
  return vkeys => () => ws.set_vkeys(vkeys);
}

export function txWitnessSetSetPlutusScripts(ws) {
  return scripts => () => ws.set_plutus_scripts(scripts);
}

export function transactionWitnessSetSetNativeScripts(ws) {
  return scripts => () => ws.set_native_scripts(scripts);
}

export function _wsSetBootstraps(helper) {
  return ws => bootstraps => () =>
    ws.set_bootstraps(helper.pack(lib.BootstrapWitnesses, bootstraps));
}

export function newBootstrapWitness(vkey) {
  return signature => chain_code => attributes => () => {
    lib.BootstrapWitness.new(vkey, signature, chain_code, attributes);
  };
}

export function _wsSetPlutusData(helper) {
  return ws => plutus_data => () =>
    ws.set_plutus_data(helper.pack(lib.PlutusList, plutus_data));
}

export function newRedeemer(tag) {
  return index => data => ex_units => () =>
    lib.Redeemer.new(tag, index, data, ex_units);
}

export function _newRedeemerTag(tag) {
  return () => lib.RedeemerTag["new_" + tag]();
}

export function newExUnits(mem) {
  return steps => lib.ExUnits.new(mem, steps);
}

export function _wsSetRedeemers(helper) {
  return ws => redeemers => () =>
    ws.set_redeemers(helper.pack(lib.Redeemers, redeemers));
}

export function _mkRedeemers(helper) {
  return redeemers => helper.pack(lib.Redeemers, redeemers);
}

export function _wsSetPlutusScripts(helper) {
  return ws => scripts => () =>
    ws.set_plutus_scripts(helper.pack(lib.PlutusScripts, scripts));
}
