/* global require exports BROWSER_RUNTIME */

const Blake2bPureJs = require('blakejs');
const Blake2bWasm = require('blake2b-wasm');

const SHA256 = require('jssha/dist/sha256');
const SHA3 = require('jssha/dist/sha3');

let lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
  lib = require('@ngua/cardano-serialization-lib-browser');
} else {
  lib = require('@ngua/cardano-serialization-lib-nodejs');
}

const DIGEST_LENGTH_256 = 32;
const DIGEST_LENGTH_224 = 28;

const withBlake2bWasmObject = bytesToHash => digestLength => {
  let obj = Blake2bWasm(digestLength);
  obj.update(Buffer.from(bytesToHash));
  return obj;
};

const blake2bHash = bytesToHash => digestLength => {
  return new Promise(resolve => {
    Blake2bWasm.ready(error => {
      if (error || !Blake2bWasm.SUPPORTED) {
        resolve(Blake2bPureJs.blake2b(bytesToHash, null, digestLength));
      } else {
        resolve(withBlake2bWasmObject(bytesToHash)(digestLength).digest());
      }
    })
  })
};

const blake2bHashHex = bytesToHash => digestLength => {
  return new Promise(resolve => {
    Blake2bWasm.ready(error => {
      if (error || !Blake2bWasm.SUPPORTED) {
        resolve(Blake2bPureJs.blake2bHex(bytesToHash, null, digestLength));
      } else {
        resolve(withBlake2bWasmObject(bytesToHash)(digestLength).digest('hex'));
      }
    })
  })
};

exports._blake2b256Hash = bytesToHash => () => {
  return blake2bHash(bytesToHash)(DIGEST_LENGTH_256);
};

exports._blake2b256HashHex = bytesToHash => () => {
  return blake2bHashHex(bytesToHash)(DIGEST_LENGTH_256);
};

exports.hashPlutusData = plutusData => {
  return lib.hash_plutus_data(plutusData).to_bytes();
};

exports.hashPlutusScript = plutusScriptBytes => () => {
  // set Plutus language namespace byte
  let bytes = new Uint8Array([0x1, ...plutusScriptBytes]);
  return blake2bHash(bytes)(DIGEST_LENGTH_224);
};

const SHA256_HASH_VARIANT = 'SHA-256';
const SHA3_256_HASH_VARIANT = 'SHA3-256';

const UINT8ARRAY_FORMAT = 'UINT8ARRAY';
const HEX_FORMAT = 'HEX';

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
