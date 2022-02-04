/* global require exports */

const lib = require('@ngua/cardano-serialization-lib-nodejs');

const fromBytes = cls => maybe => bytes => {
    try {
        return maybe.just(cls.from_bytes(bytes));
    } catch (_) {
        return maybe.nothing;
    }
};

const call = property => object => object[property]();
const callMaybe = property => maybe => object => {
    const res = object[property]();
    return res ? maybe.just(res) : maybe.nothing;
};

exports._baseAddressFromAddress = maybe => address => {
    const res = lib.BaseAddress.from_address(address);
    return res ? maybe.just(res) : maybe.nothing;
};

exports.addressNetworkId = call('network_id');
exports.baseAddressStakeCredential = call('stake_cred');
exports.baseAddressPaymentCredential = call('payment_cred');
exports._stakeCredentialToScriptHash = callMaybe('to_scripthash');
exports._stakeCredentialToKeyHash = callMaybe('to_keyhash');
