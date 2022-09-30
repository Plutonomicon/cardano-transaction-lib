/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const hashFromImpl = hashClassFrom => maybe => input => {
  let ret = null;
  try {
    ret = hashClassFrom(input);
  } catch (e) {
    console.log(e);
  }
  if (ret == null) {
    return maybe.nothing;
  }
  return maybe.just(ret);
};

exports.hashToBytes = hash => {
  return hash.to_bytes();
};

exports.hashToBech32Unsafe = prefix => hash => {
  return hash.to_bech32(prefix);
};

exports.hashToBech32Impl = maybe => prefix => hash => {
  let ret = null;
  try {
    ret = hash.to_bech32(prefix);
  } catch (e) {
    console.log(e);
  }
  if (ret == null) {
    return maybe.nothing;
  }
  return maybe.just(ret);
};

exports._ed25519KeyHashFromBech32Impl = maybe => bech32str => {
  return hashFromImpl(lib.Ed25519KeyHash.from_bech32)(maybe)(bech32str);
};

exports._ed25519KeyHashFromBytesImpl = maybe => bytes => {
  return hashFromImpl(lib.Ed25519KeyHash.from_bytes)(maybe)(bytes);
};

exports._scriptHashFromBytesImpl = maybe => bytes => {
  return hashFromImpl(lib.ScriptHash.from_bytes)(maybe)(bytes);
};

exports._scriptHashFromBech32Impl = maybe => bech32str => {
  return hashFromImpl(lib.ScriptHash.from_bech32)(maybe)(bech32str);
};

exports.nativeScriptHash = script => script.hash();
