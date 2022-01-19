var lib = // require('@emurgo/cardano-serialization-lib-nodejs');
    require('../node_modules/cardano-serialization-lib/publish/cardano_serialization_lib.js');

exports.newTransactionBody = inputs => outputs => fee => () =>
    lib.TransactionBody.new(inputs, outputs, fee /* ttl */);

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
    lib.TransactionHash.from_bech32(hash);

exports.newTransactionInput = transaction_id => index => () =>
    lib.TransactionInput.new(transaction_id, index);

exports.newTransactionOutput = address => amount => () =>
    lib.TransactionOutput.new(address, amount);

exports.newTransactionWitnessSet = () =>
    lib.TransactionWitnessSet.new();

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

exports.newEd25519KeyHash = bech_str => () =>
    lib.Ed25519KeyHash.from_bech32(bech_str);

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

exports.newDataHash = bech32 => () =>
    // TODO: is correct representation?
    lib.DataHash.from_bech32(bech32);

exports.transactionOutputSetDataHash = output => hash => () =>
    output.set_data_hash(hash);
