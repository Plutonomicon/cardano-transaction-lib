/* global require exports */

const lib = require('@ngua/cardano-serialization-lib-nodejs');

const fromBytes = cls => Nothing => Just => bytes => {
    try {
        return Just(cls.from_bytes(bytes));
    } catch (_) {
        return Nothing;
    }
};

const call = property => object => object[property]();
const callMaybe = property => Nothing => Just => object => {
    const res = object[property]();
    return res ? Just(res) : Nothing;
};

exports.toBech32 = call('to_bech32');

exports._baseAddressFromAddress = Nothing => Just => address => {
    const res = lib.BaseAddress.from_address(address);
    return res ? Just(res) : Nothing;
};

exports.addressNetworkId = call('network_id');
exports.baseAddressStakeCredential = call('stake_cred');
exports.baseAddressPaymentCredential = call('payment_cred');
exports._stakeCredentialToScriptHash = callMaybe('to_scripthash');
exports._stakeCredentialToKeyHash = callMaybe('to_keyhash');
