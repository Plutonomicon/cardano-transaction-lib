var lib = // require('@emurgo/cardano-serialization-lib-nodejs');
    require('../node_modules/cardano-serialization-lib/publish/cardano_serialization_lib.js');

exports.newTransactionBody = function (inputs) {
    return function (outputs) {
        return function (fee) {
            return function () {
                lib.TransactionBody.new(inputs, outputs, fee /* ttl */);
            };
        };
    };
};

exports.newTransaction = function (body) {
    return function (witness_set) {
        return function () {
            return lib.Transaction.new(body, witness_set /* auxiliary_data */);
        };
    };
};

exports.newTransaction_ = function (body) {
    return function (witness_set) {
        return function (auxiliary_data) {
            return function () {
                return lib.Transaction.new(body, witness_set, auxiliary_data);
            };
        };
    };
};

exports.newTransactionInputs = function () {
    return lib.TransactionInputs.new();
};

exports.addTransactionInput = function (inputs) {
    return function (input) {
        return function () {
            inputs.add(input);
        };
    };
};

exports.newTransactionOutputs = function () {
    return lib.TransactionOutputs.new();
};

exports.addTransactionOutput = function (outputs) {
    return function (output) {
        return function () {
            outputs.add(output);
        };
    };
};

exports.newTransactionHash = function (hash) {
    return function () {
        return lib.TransactionHash.from_bech32(hash);
    };
};

exports.newTransactionInput = function (transaction_id) {
    return function (index) {
        return function () {
            return lib.TransactionInput.new(transaction_id, index);
        };
    };
};

exports.newTransactionOutput = function (address) {
    return function (amount) {
        return function () {
            return lib.TransactionOutput.new(address, amount);
        };
    };
};

exports.newTransactionWitnessSet = function () {
    return lib.TransactionWitnessSet.new();
};

exports.newAddress = function (bech_str) {
    return function () {
        return lib.Address.from_bech32(bech_str);
    };
};

exports.newBigNum = function (string) {
    return function () {
        return lib.BigNum.new(string);
    };
};

exports.newValue = function (coin) {
    return function () {
        return lib.Value.new(coin);
    };
};

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
