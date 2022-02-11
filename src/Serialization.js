/* global require exports */

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

exports.newAddressFromBech32 = bech32 => () =>
    lib.Address.from_bech32(bech32);

exports.newAddressFromBytes = bytes => () =>
    lib.Address.from_bytes(bytes);

exports.newBaseAddress = network => payment => stake => () =>
    lib.BaseAddress.new(network, payment, stake);

exports._newBaseAddressFromAddress = nothing => just => address => {
    const res = lib.BaseAddress.from_address(address);
    return res ? just(res) : nothing;
};

exports.baseAddressPaymentCredential =
    baseAddress => baseAddress.payment_cred();

exports.baseAddressToAddress = baseAddress => () =>
    baseAddress.to_address();

exports.newStakeCredentialFromScriptHash = hash => () =>
    lib.StakeCredential.from_scripthash(hash);

exports.newStakeCredentialFromKeyHash = hash => () =>
    lib.StakeCredential.from_keyhash(hash);

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

exports._addressPubKeyHash = just => nothing => baseAddr => {
    // i've chosen a prefix that Nami uses for payment_creds
    const kh = baseAddr.payment_cred().to_keyhash();
    if(kh==null){
        return nothing;
    }
    return just(kh.to_bech32('hbas_'));
};

exports._addressBech32 = addr => {
  return addr.to_bech32();
};

exports.toBytes = sth => sth.to_bytes();
