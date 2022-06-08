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

const blake2b256HashPureJs = bytesToHash => {
  return Blake2bPureJs.blake2b(bytesToHash, null, DIGEST_LENGTH_256);
};

const blake2b256HashHexPureJs = bytesToHash => {
  return Blake2bPureJs.blake2bHex(bytesToHash, null, DIGEST_LENGTH_256);
};

const withBlake2bWasmObject = bytesToHash => {
  let obj = Blake2bWasm(DIGEST_LENGTH_256);
  obj.update(Buffer.from(bytesToHash));
  return obj;
};

exports._blake2b256Hash = bytesToHash => () => {
  return new Promise(resolve => {
    Blake2bWasm.ready(error => {
      if (error || !Blake2bWasm.SUPPORTED) {
        resolve(blake2b256HashPureJs(bytesToHash));
      } else {
        resolve(withBlake2bWasmObject(bytesToHash).digest());
      }
    })
  })
};

exports._blake2b256HashHex = bytesToHash => () => {
  return new Promise(resolve => {
    Blake2bWasm.ready(error => {
      if (error || !Blake2bWasm.SUPPORTED) {
        resolve(blake2b256HashHexPureJs(bytesToHash));
      } else {
        resolve(withBlake2bWasmObject(bytesToHash).digest('hex'));
      }
    })
  })
};

exports.hashPlutusData = plutusData =>
    lib.hash_plutus_data(plutusData).to_bytes();

exports.hashPlutusScript = plutusScriptBytes => {
    // set Plutus language namespace byte
    let bytes = new Uint8Array([0x1, ...plutusScriptBytes]);
    return Blake2bPureJs.blake2b(bytes, null, DIGEST_LENGTH_224);
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
