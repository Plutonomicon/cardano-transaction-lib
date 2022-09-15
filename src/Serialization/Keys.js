/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports._publicKeyFromBech32 = maybe => bech32 => {
  try {
    return maybe.just(lib.PublicKey.from_bech32(bech32));
  } catch (_) {
    return maybe.nothing;
  }
};

const bytesFromKey = maybe => key => {
  try {
    return maybe.just(key.as_bytes());
  } catch (err) {
    return maybe.nothing;
  }
};

exports._bytesFromPublicKey = bytesFromKey;
exports._bytesFromPrivateKey = bytesFromKey;

exports.publicKeyFromPrivateKey = private_key => () => {
  return private_key.to_public();
};

exports._privateKeyFromBytes = maybe => bytes => {
  try {
    return maybe.just(lib.PrivateKey.from_normal_bytes(bytes));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._publicKeyFromBytes = maybe => bytes => {
  try {
    return maybe.just(lib.PublicKey.from_bytes(bytes));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._bech32FromPublicKey = key => () => key.to_bech32();
