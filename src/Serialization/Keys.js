/* global BROWSER_RUNTIME */

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

const bech32FromKey = key => () => key.to_bech32();

exports._bech32FromPublicKey = bech32FromKey;
exports._bech32FromPrivateKey = bech32FromKey;
