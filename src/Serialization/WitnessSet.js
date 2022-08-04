/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports.newTransactionWitnessSet = () => lib.TransactionWitnessSet.new();

exports.newEd25519Signature = bech32 => () =>
  lib.Ed25519Signature.from_bech32(bech32);

exports.newPublicKey = bech32 => () => lib.PublicKey.from_bech32(bech32);

exports.newVkeyFromPublicKey = public_key => () => lib.Vkey.new(public_key);

exports.newVkeywitnesses = () => lib.Vkeywitnesses.new();

exports.newVkeywitness = vkey => signature => () =>
  lib.Vkeywitness.new(vkey, signature);

exports.addVkeywitness = witnesses => witness => () => witnesses.add(witness);

exports.newPlutusScripts = () => lib.PlutusScripts.new();

exports.addPlutusScript = scripts => script => () => scripts.add(script);

exports.transactionWitnessSetSetVkeys = ws => vkeys => () =>
  ws.set_vkeys(vkeys);

exports.txWitnessSetSetPlutusScripts = ws => scripts => () =>
  ws.set_plutus_scripts(scripts);

exports.transactionWitnessSetSetNativeScripts = ws => scripts => () =>
  ws.set_native_scripts(scripts);

exports._wsSetBootstraps = helper => ws => bootstraps => () =>
  ws.set_bootstraps(helper.pack(lib.BootstrapWitnesses, bootstraps));

exports.newBootstrapWitness =
  vkey => signature => chain_code => attributes => () => {
    lib.BootstrapWitness.new(vkey, signature, chain_code, attributes);
  };

exports._wsSetPlutusData = helper => ws => plutus_data => () =>
  ws.set_plutus_data(helper.pack(lib.PlutusList, plutus_data));

exports.newRedeemer = tag => index => data => ex_units => () =>
  lib.Redeemer.new(tag, index, data, ex_units);

exports._newRedeemerTag = tag => () => lib.RedeemerTag["new_" + tag]();

exports.newExUnits = mem => steps => lib.ExUnits.new(mem, steps);

exports._wsSetRedeemers = helper => ws => redeemers => () =>
  ws.set_redeemers(helper.pack(lib.Redeemers, redeemers));

exports._mkRedeemers = helper => redeemers =>
  helper.pack(lib.Redeemers, redeemers);

exports._wsSetPlutusScripts = helper => ws => scripts => () =>
  ws.set_plutus_scripts(helper.pack(lib.PlutusScripts, scripts));
