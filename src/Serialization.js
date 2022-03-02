/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

exports.newBigNum = string => () =>
    lib.BigNum.from_str(string);

exports.newValue = coin => () =>
    lib.Value.new(coin);

exports.newValueFromAssets = multiasset => () =>
    lib.Value.new_from_assets(multiasset);

exports.valueSetCoin = value => coin => () =>
    value.set_coin(coin);

exports.newTransactionInput = transaction_id => index => () =>
    lib.TransactionInput.new(transaction_id, index);

exports.newTransactionInputs = () =>
    lib.TransactionInputs.new();

exports.addTransactionInput = inputs => input => () =>
    inputs.add(input);

exports.newTransactionOutput = address => amount => () =>
    lib.TransactionOutput.new(address, amount);

exports.newTransactionOutputs = () =>
    lib.TransactionOutputs.new();

exports.addTransactionOutput = outputs => output => () =>
    outputs.add(output);

exports.newTransactionBody = inputs => outputs => fee => () =>
    lib.TransactionBody.new(inputs, outputs, fee);

exports.newTransaction = body => witness_set => () =>
    lib.Transaction.new(body, witness_set);

exports.newTransaction_ = body => witness_set => auxiliary_data => () =>
    lib.Transaction.new(body, witness_set, auxiliary_data);

exports.newTransactionWitnessSet = () =>
    lib.TransactionWitnessSet.new();

exports.newTransactionUnspentOutputFromBytes = bytes => () =>
    lib.TransactionUnspentOutput.from_bytes(bytes);

exports.newTransactionWitnessSetFromBytes = bytes => () =>
    lib.TransactionWitnessSet.from_bytes(bytes);

exports.newMultiAsset = () =>
    lib.MultiAsset.new();

exports.insertMultiAsset = multiasset => key => value => () =>
    multiasset.insert(key, value);

exports.newAssets = () =>
    lib.Assets.new();

exports.insertAssets = assets => key => value => () =>
    assets.insert(key, value);

exports.newAssetName = name => () =>
    lib.AssetName.new(name);

exports.transactionOutputSetDataHash = output => hash => () =>
    output.set_data_hash(hash);

exports.newVkeywitnesses = () =>
    lib.Vkeywitnesses.new();

exports.newVkeywitness = vkey => signature => () =>
    lib.Vkeywitness.new(vkey, signature);

exports.addVkeywitness = witnesses => witness => () =>
    witnesses.add(witness);

exports.newVkeyFromPublicKey = public_key => () =>
    lib.Vkey.new(public_key);

exports.newPublicKey = bech32 => () =>
    lib.PublicKey.from_bech32(bech32);

exports.newEd25519Signature = bech32 => () =>
    lib.Ed25519Signature.from_bech32(bech32);

exports.transactionWitnessSetSetVkeys = ws => vkeys => () =>
    ws.set_vkeys(vkeys);

exports.newPlutusScript = bytes => () =>
    lib.PlutusScript.new(bytes);

exports.newPlutusScripts = bytes => () =>
    lib.PlutusScripts.new(bytes);

exports.txWitnessSetSetPlutusScripts = ws => scripts => () =>
    ws.set_plutus_scripts(scripts);

exports.addPlutusScript = scripts => script => () =>
    scripts.add(script);

exports.toBytes = sth => sth.to_bytes();

exports.newCostmdls = () =>
    lib.Costmdls.new();

exports.costmdlsSetCostModel = cms => lang => cm => () =>
    cms.insert(lang, cm);

exports.newCostModel = () =>
    lib.CostModel.new();

exports.costModelSetCost = cm => op => cost => () =>
    cm.set(op, cost);

exports.newPlutusV1 = () =>
    lib.Language.new_plutus_v1();

exports.newInt32 = x => () =>
    lib.Int.new_i32(x);
