/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports.hashToBytes = hash => {
  return hash.to_bytes();
};

exports.hashFromBytes = name => maybe => bytes => {
  return hashFromImpl(lib[name].from_bytes)(maybe)(bytes);
};

exports.hashToBech32Unsafe = prefix => hash => {
  return hash.to_bech32(prefix);
};

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

exports._scriptHashFromBech32Impl = maybe => bech32str => {
  return hashFromImpl(lib.ScriptHash.from_bech32)(maybe)(bech32str);
};

exports.nativeScriptHash = script => script.hash();
