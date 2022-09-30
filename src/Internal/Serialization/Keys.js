/* global BROWSER_RUNTIME */

const bytesFromKey = key => key.as_bytes();

exports.bytesFromPublicKey = bytesFromKey;
exports.bytesFromPrivateKey = bytesFromKey;

exports.publicKeyFromPrivateKey = private_key => () => private_key.to_public();

const bech32FromX = key => key.to_bech32();

exports.bech32FromPublicKey = bech32FromX;
exports.bech32FromPrivateKey = bech32FromX;
exports.bech32FromEd25519Signature = bech32FromX;
