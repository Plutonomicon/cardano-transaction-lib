/* global BROWSER_RUNTIME */

const Blake2bWasm = require("blake2b-wasm");
const SHA256 = require("jssha/dist/sha256");
const SHA3 = require("jssha/dist/sha3");

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports._blake2bReady = () => {
  return new Promise((resolve, reject) => {
    Blake2bWasm.ready(error => {
      if (error || !Blake2bWasm.SUPPORTED) {
        reject(new Error("Could not initialize blake2b-wasm"));
      } else {
        resolve();
      }
    });
  });
};

// -----------------------------------------------------------------------------
// blake2b256Hash, blake2b256HashHex, hashPlutusData, hashPlutusScript
// -----------------------------------------------------------------------------

const blake2b256Hash = bytesToHash => digestEncoding => {
  try {
    return Blake2bWasm(32)
      .update(Buffer.from(bytesToHash))
      .digest(digestEncoding);
  } catch (_) {
    throw new Error("WASM not loaded. Use `blake2bReady` before hashing.");
  }
};

exports.unsafeBlake2b256Hash = bytesToHash => {
  return blake2b256Hash(bytesToHash)("binary");
};

exports.unsafeBlake2b256HashHex = bytesToHash => {
  return blake2b256Hash(bytesToHash)("hex");
};

exports.hashPlutusData = plutusData => {
  return lib.hash_plutus_data(plutusData).to_bytes();
};

exports.plutusScriptHash = script => {
  return lib.PlutusScript.new(script).hash();
};

// -----------------------------------------------------------------------------
// sha256Hash, sha256HashHex, sha3_256Hash, sha3_256HashHex
// -----------------------------------------------------------------------------

const SHA256_HASH_VARIANT = "SHA-256";
const SHA3_256_HASH_VARIANT = "SHA3-256";
const UINT8ARRAY_FORMAT = "UINT8ARRAY";
const HEX_FORMAT = "HEX";

exports.sha256Hash = bytesToHash => {
  const shaObj = new SHA256(SHA256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(UINT8ARRAY_FORMAT);
};

exports.sha256HashHex = bytesToHash => {
  const shaObj = new SHA256(SHA256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(HEX_FORMAT);
};

exports.sha3_256Hash = bytesToHash => {
  const shaObj = new SHA3(SHA3_256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(UINT8ARRAY_FORMAT);
};

exports.sha3_256HashHex = bytesToHash => {
  const shaObj = new SHA3(SHA3_256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(HEX_FORMAT);
};
