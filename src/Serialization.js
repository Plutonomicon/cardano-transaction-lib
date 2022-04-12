/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

exports.newBigNum = maybe => string => {
    try {
        return maybe.just(lib.BigNum.from_str(string));
    } catch (_) {
        return maybe.nothing;
    }
};

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

exports.newTransactionBody = inputs => outputs => fee => ttl => () =>
    lib.TransactionBody.new(inputs, outputs, fee, ttl);

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

exports._publicKeyFromBech32 = maybe => bech32 => () => {
    try {
        return maybe.just(lib.PublicKey.from_bech32(bech32));
    } catch (_) {
        return maybe.nothing;
    }
};

exports.publicKeyHash = pk => pk.hash();

exports.newEd25519Signature = bech32 => () =>
    lib.Ed25519Signature.from_bech32(bech32);

exports.transactionWitnessSetSetVkeys = ws => vkeys => () =>
    ws.set_vkeys(vkeys);

exports.newPlutusScript = bytes => () =>
    lib.PlutusScript.from_bytes(bytes);

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

exports._hashScriptData = rs => cms => ds => () =>
    lib.hash_script_data(rs, cms, ds);

exports.newRedeemers = () =>
    lib.Redeemers.new();

exports.addRedeemer = rs => r => () =>
    rs.add(r);

exports.newScriptDataHashFromBytes = bytes => () =>
    lib.ScriptDataHash.from_bytes(bytes);

exports.setTxBodyScriptDataHash = body => sdh => () =>
    body.set_script_data_hash(sdh);

exports.setTxBodyMint = body => mint => () =>
    body.set_mint(mint);

exports.newMint = () =>
    lib.Mint.new();

exports._bigIntToInt = maybeFfiHelper => bigInt => {
    try {
        const str = bigInt.to_str();
        if (str[0] == '-') {
            return maybeFfiHelper.just(
                lib.Int.new_negative(lib.BigNum.from_str(str.slice(1)))
            );
        } else {
            return maybeFfiHelper.just(
                lib.Int.new(lib.BigNum.from_str(str))
            );
        }
    } catch (_) {
        return Maybe.nothing;
    }
};

exports.newMintAssets = lib.MintAssets.new;

exports.insertMintAssets = mint => scriptHash => mintAssets => () =>
    mint.insert(scriptHash, mintAssets);

exports.insertMintAsset = mintAssets => assetName => int => () =>
    mintAssets.insert(assetName, int);

exports.networkIdTestnet = () =>
    lib.NetworkId.testnet();

exports.networkIdMainnet = () =>
    lib.NetworkId.mainnet();
exports.setTxBodyCollateral = body => inputs => () =>
    body.set_collateral(inputs);

exports.setTxBodyNetworkId = body => network_id => () =>
    body.set_network_id(network_id);

exports.transactionBodySetRequiredSigners = containerHelper => body =>
    keyHashes => () =>
    body.set_required_signers(
        containerHelper.pack(lib.Ed25519KeyHashes, keyHashes));

exports.transactionBodySetValidityStartInterval =
    txBody => validityStartInterval => () =>
    txBody.set_validity_start_interval(validityStartInterval);
