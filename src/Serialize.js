var lib = // require('@emurgo/cardano-serialization-lib-nodejs');
    require('../node_modules/cardano-serialization-lib/publish/cardano_serialization_lib.js');

exports.newTransactionBody = inputs => outputs => fee => () =>
    lib.TransactionBody.new(inputs, outputs, fee /* ttl */);

exports.newTransactionBody_ = inputs => outputs => fee => ttl =>
    lib.TransactionBody.new(inputs, outputs, fee, ttl);

exports.newTransaction = body => witness_set => () =>
    lib.Transaction.new(body, witness_set /* auxiliary_data */);

exports.newTransaction_ = body => witness_set => auxiliary_data => () =>
    lib.Transaction.new(body, witness_set, auxiliary_data);

exports.newTransactionInputs = () =>
    lib.TransactionInputs.new();

exports.addTransactionInput = inputs => input => () =>
    inputs.add(input);

exports.newTransactionOutputs = () =>
    lib.TransactionOutputs.new();

exports.addTransactionOutput = outputs => output => () =>
    outputs.add(output);

exports.newTransactionHash = hash => () =>
    lib.TransactionHash.from_bytes(hash);

exports.newTransactionInput = transaction_id => index => () =>
    lib.TransactionInput.new(transaction_id, index);

exports.newTransactionOutput = address => amount => () =>
    lib.TransactionOutput.new(address, amount);

exports.newTransactionWitnessSet = () =>
    lib.TransactionWitnessSet.new();

exports.transactionWitnessSetSetVkeys = ws => vkeys => () =>
    ws.set_vkeys(vkeys);

exports.newBigNum = string => () =>
    lib.BigNum.from_str(string);

exports.newValue = coin => () =>
    lib.Value.new(coin);

exports.newBaseAddress = network => payment => stake => () =>
    lib.BaseAddress.new(network, payment, stake);

exports.baseAddressToAddress = baseAddress => () => baseAddress.to_address();

exports.newStakeCredentialFromScriptHash = hash => () =>
    lib.StakeCredential.from_scripthash(hash);

exports.newStakeCredentialFromKeyHash = hash => () =>
    lib.StakeCredential.from_keyhash(hash);

exports.newEd25519KeyHash = bytes => () =>
    lib.Ed25519KeyHash.from_bytes(bytes);

exports.newValueFromAssets = multiasset => () =>
    lib.Value.new_from_assets(multiasset);

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

exports.newScriptHash = array => () =>
    lib.ScriptHash.from_bytes(array);

exports.newDataHash = bytes => () =>
    lib.DataHash.from_bytes(bytes);

exports.transactionOutputSetDataHash = output => hash => () =>
    output.set_data_hash(hash);

exports.newVkeywitnesses = () =>
    lib.Vkeywitnesses.new();

exports.addVkeywitness = witnesses => witness => () =>
    wintesses.add(witness);

exports.newVkeywitness = vkey => signature => () =>
    lib.Vkeywitness.new(vkey, signature);

exports.newVkeyFromPublicKey = public_key => () =>
    lib.Vkey.new(public_key);

exports.newPublicKey = bech32 => () =>
    lib.PublicKey.from_bech32(bech32);

exports.newEd25519Signature = bech32 => () =>
    lib.Ed25519Signature.from_bech32(bech32);

exports.newPlutusScript = bytes => () =>
    lib.PlutusScript.new(bytes);

exports.newPlutusScripts = bytes => () =>
    lib.PlutusScripts.new(bytes);

exports.addPlutusScript = scripts => script => () =>
    scripts.add(script);

exports.txWitnessSetSetPlutusScripts = ws => scripts => () =>
    ws.set_plutus_scripts(scripts);
