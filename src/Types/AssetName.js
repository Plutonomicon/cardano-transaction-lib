/* global BROWSER_RUNTIME */

// `TextDecoder` is not available in `node`, use polyfill in that case
let OurTextDecoder;
if (typeof BROWSER_RUNTIME == "undefined" || !BROWSER_RUNTIME) {
  const util = await import("util");
  OurTextDecoder = util.TextDecoder;
} else {
  OurTextDecoder = TextDecoder;
}

export function _decodeUtf8(buffer) {
  return left => right => {
    let decoder = new OurTextDecoder("utf-8", { fatal: true }); // Without fatal=true it never fails

    try {
      return right(decoder.decode(buffer));
    } catch (err) {
      return left(err.toString());
    }
  };
}

// FIXME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/548
const call = property => object => object[property]();
export const assetNameName = call("name");
