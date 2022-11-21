const base64 = require("base64-js");

exports.fromByteArray = base64.fromByteArray;

exports.toByteArray = base64.toByteArray;

exports._decodeBase64 = maybe => str => {
  try {
    return maybe.just(base64.toByteArray(str));
  } catch (_) {
    return maybe.nothing;
  }
};
