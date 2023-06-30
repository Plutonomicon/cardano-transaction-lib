import base64 from "base64-js";
export var fromByteArray = base64.fromByteArray;
export var toByteArray = base64.toByteArray;

export function _decodeBase64(maybe) {
  return str => {
    try {
      return maybe.just(base64.toByteArray(str));
    } catch (_) {
      return maybe.nothing;
    }
  };
}
