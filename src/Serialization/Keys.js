/* global BROWSER_RUNTIME */

const bytesFromKey = key => key.as_bytes();

exports.bytesFromPublicKey = bytesFromKey;
exports.bytesFromPrivateKey = bytesFromKey;

exports.publicKeyFromPrivateKey = private_key => () => {
  return private_key.to_public();
};

const bech32FromKey = key => key.to_bech32();

exports._bech32FromPublicKey = bech32FromKey;
exports._bech32FromPrivateKey = bech32FromKey;
