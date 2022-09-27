/* global BROWSER_RUNTIME */

const Blake2 = require("blakejs");
const SHA256 = require("jssha/dist/sha256");
const SHA3 = require("jssha/dist/sha3");

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports.blake2b256Hash = bytesToHash => {
  return Blake2.blake2b(bytesToHash, null, 32);
};

exports.blake2b256HashHex = bytesToHash => {
  return Blake2.blake2bHex(bytesToHash, null, 32);
};

exports.hashPlutusData = plutusData => {
  return lib.hash_plutus_data(plutusData).to_bytes();
};

exports.hashPlutusScript = script => script.hash();

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
