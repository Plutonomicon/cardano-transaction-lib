import Blake2 from "blakejs";
import SHA256 from "jssha/sha256";
import SHA3 from "jssha/sha3";
import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

export function blake2b224Hash(bytesToHash) {
  return Blake2.blake2b(bytesToHash, null, 28);
}

export function blake2b224HashHex(bytesToHash) {
  return Blake2.blake2bHex(bytesToHash, null, 28);
}

export function blake2b256Hash(bytesToHash) {
  return Blake2.blake2b(bytesToHash, null, 32);
}

export function blake2b256HashHex(bytesToHash) {
  return Blake2.blake2bHex(bytesToHash, null, 32);
}

export function hashPlutusData(plutusData) {
  return lib.hash_plutus_data(plutusData);
}

export function hashPlutusScript(script) {
  return script.hash();
}

const SHA256_HASH_VARIANT = "SHA-256";
const SHA3_256_HASH_VARIANT = "SHA3-256";

const UINT8ARRAY_FORMAT = "UINT8ARRAY";
const HEX_FORMAT = "HEX";

export function sha256Hash(bytesToHash) {
  const shaObj = new SHA256(SHA256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(UINT8ARRAY_FORMAT);
}

export function sha256HashHex(bytesToHash) {
  const shaObj = new SHA256(SHA256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(HEX_FORMAT);
}

export function sha3_256Hash(bytesToHash) {
  const shaObj = new SHA3(SHA3_256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(UINT8ARRAY_FORMAT);
}

export function sha3_256HashHex(bytesToHash) {
  const shaObj = new SHA3(SHA3_256_HASH_VARIANT, UINT8ARRAY_FORMAT);
  shaObj.update(bytesToHash);
  return shaObj.getHash(HEX_FORMAT);
}
