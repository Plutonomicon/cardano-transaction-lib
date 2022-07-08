/* global require exports BROWSER_RUNTIME */

const Blake2bWasm = require("blake2b-wasm");
const SHA256 = require("jssha/dist/sha256");
const SHA3 = require("jssha/dist/sha3");

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

// -----------------------------------------------------------------------------
// blake2b256Hash, blake2b256HashHex, hashPlutusData, hashPlutusScript
// -----------------------------------------------------------------------------

const DIGEST_LENGTH_256 = 32;
const DIGEST_LENGTH_224 = 28;
const DIGEST_ENCODING_BINARY = "binary";
const DIGEST_ENCODING_HEX = "hex";

const blake2bHash = (bytesToHash) => (digestLength) => (digestEncoding) => {
  return new Promise((resolve, reject) => {
    Blake2bWasm.ready((error) => {
      if (error || !Blake2bWasm.SUPPORTED) {
        reject(new Error("Failed to calculate Blake2b hash"));
      } else {
        const digest = Blake2bWasm(digestLength)
          .update(Buffer.from(bytesToHash))
          .digest(digestEncoding);
        resolve(digest);
      }
    });
  });
};

exports._blake2b256Hash = (bytesToHash) => () => {
  return blake2bHash(bytesToHash)(DIGEST_LENGTH_256)(DIGEST_ENCODING_BINARY);
};

exports._blake2b256HashHex = (bytesToHash) => () => {
  return blake2bHash(bytesToHash)(DIGEST_LENGTH_256)(DIGEST_ENCODING_HEX);
};

exports.hashPlutusData = (plutusData) => {
  return lib.hash_plutus_data(plutusData).to_bytes();
};

exports.hashPlutusScript = (plutusScriptBytes) => () => {
  // set Plutus language namespace byte
  const bytes = new Uint8Array([0x1, ...plutusScriptBytes]);
  return blake2bHash(bytes)(DIGEST_LENGTH_224)(DIGEST_ENCODING_BINARY);
};

// -----------------------------------------------------------------------------
// sha256Hash, sha256HashHex, sha3_256Hash, sha3_256HashHex
// -----------------------------------------------------------------------------

const SHA256_HASH_VARIANT = "SHA-256";
const SHA3_256_HASH_VARIANT = "SHA3-256";
const UINT8ARRAY_FORMAT = "UINT8ARRAY";
const HEX_FORMAT = "HEX";

exports.sha256Hash = (bytesToHash) => {
  const shaObj = new SHA256(SHA256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(UINT8ARRAY_FORMAT);
};

exports.sha256HashHex = (bytesToHash) => {
  const shaObj = new SHA256(SHA256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(HEX_FORMAT);
};

exports.sha3_256Hash = (bytesToHash) => {
  const shaObj = new SHA3(SHA3_256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(UINT8ARRAY_FORMAT);
};

exports.sha3_256HashHex = (bytesToHash) => {
  const shaObj = new SHA3(SHA3_256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(HEX_FORMAT);
};
