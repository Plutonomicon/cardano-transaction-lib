/* global require exports */

const lib = require('@ngua/cardano-serialization-lib-nodejs');

const call = property => object => object[property]();
const callMaybe = property => maybe => object => {
    const res = object[property]();
    return res != null ? maybe.just(res) : maybe.nothing;
};

exports._baseAddressFromAddress = maybe => address => {
    const res = lib.BaseAddress.from_address(address);
    return res != null ? maybe.just(res) : maybe.nothing;
};

exports.addressNetworkId = call('network_id');
exports.baseAddressStakeCredential = call('stake_cred');
exports.baseAddressPaymentCredential = call('payment_cred');
exports._stakeCredentialToScriptHash = callMaybe('to_scripthash');
exports._stakeCredentialToKeyHash = callMaybe('to_keyhash');
