const CardanoWasm = require("@emurgo/cardano-serialization-lib-nodejs");

const hashFromImpl = hashClassFrom => maybe => input => {
    const ret = null;
    try {
        ret = hashClassFrom(input);
    }
    catch (e) {
        console.log(e);
    }
    if (ret == null) {
        return maybe.nothing;
    }
    return maybe.just(ret);
};

const hashToBytes = hash => {
    return hash.to_bytes();
};

const hashToBech32Unsafe = prefix => hash => {
    return hash.to_bech32(prefix);
};

const hashToBech32Impl = maybe => prefix => hash => {
    const ret = null;
    try {
        ret = hash.to_bech32(prefix);
    }
    catch (e) {
        console.log(e);
    }
    if (ret == null) {
        return maybe.nothing;
    }
    return maybe.just(ret);
};

exports.ed25519KeyHashFromBech32Impl_ = maybe => bech32str => {
    return hashFromImpl(CardanoWasm.Ed25519KeyHash.from_bech32)(maybe)(bech32str);
};

exports.ed25519KeyHashFromBytesImpl_ = maybe => bytes => {
    return hashFromImpl(CardanoWasm.Ed25519KeyHash.from_bytes)(maybe)(bytes);
};

exports.scriptHashFromBytesImpl_ = maybe => bytes => {
    return hashFromImpl(CardanoWasm.ScriptHash.from_bytes)(maybe)(bytes);
};

exports.scriptHashFromBech32Impl_ = maybe => bech32str => {
    return hashFromImpl(CardanoWasm.ScriptHash.from_bech32)(maybe)(bech32str);
};

exports.ed25519KeyHashToBytes_ = hashToBytes;
exports.ed25519KeyHashToBech32Unsafe_ = hashToBech32Unsafe;
exports.ed25519KeyHashToBech32Impl_ = hashToBech32Impl;

exports.scriptHashToBytes_ = hashToBytes;
exports.scriptHashToBech32Unsafe_ = hashToBech32Unsafe;
exports.scriptHashToBech32Impl_ = hashToBech32Impl;
