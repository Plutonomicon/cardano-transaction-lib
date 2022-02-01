const CardanoWasm = require("@ngua/cardano-serialization-lib-nodejs");

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

exports.ed25519KeyHashFromBech32Impl = maybe => bech32str => {
    return hashFromImpl(CardanoWasm.Ed25519KeyHash.from_bech32)(maybe)(bech32str);
};

exports.ed25519KeyHashFromBytesImpl = maybe => bytes => {
    return hashFromImpl(CardanoWasm.Ed25519KeyHash.from_bytes)(maybe)(bytes);
};

exports.scriptHashFromBytesImpl = maybe => bytes => {
    return hashFromImpl(CardanoWasm.ScriptHash.from_bytes)(maybe)(bytes);
};

exports.scriptHashFromBech32Impl = maybe => bech32str => {
    return hashFromImpl(CardanoWasm.ScriptHash.from_bech32)(maybe)(bech32str);
};

exports.ed25519KeyHashToBytes = hashToBytes;
exports.ed25519KeyHashToBech32Unsafe = hashToBech32Unsafe;
exports.ed25519KeyHashToBech32Impl = hashToBech32Impl;

exports.scriptHashToBytes = hashToBytes;
exports.scriptHashToBech32Unsafe = hashToBech32Unsafe;
exports.scriptHashToBech32Impl = hashToBech32Impl;
