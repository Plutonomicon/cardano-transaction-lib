/* global require exports BROWSER_RUNTIME */

const Blake2 = require('blakejs');

let lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

const DIGEST_LENGTH_256 = 32;

const DIGEST_LENGTH_224 = 28;

exports.blake2b256Hash = bytesToHash =>
    Blake2.blake2b(bytesToHash, null, DIGEST_LENGTH_256);

exports.blake2b256HashHex = bytesToHash =>
    Blake2.blake2bHex(bytesToHash, null, DIGEST_LENGTH_256);

exports.hashPlutusData = plutusData =>
    lib.hash_plutus_data(plutusData).to_bytes();

exports.hashPlutusScript = plutusScriptBytes => {
    // set Plutus language namespace byte
    let bytes = new Uint8Array([0x1, ...plutusScriptBytes]);
    return Blake2.blake2b(bytes, null, DIGEST_LENGTH_224);
};
