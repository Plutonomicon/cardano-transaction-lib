import { TextDecoder } from "web-encoding";

export function _decodeUtf8(buffer) {
  return left => right => {
    let decoder = new TextDecoder("utf-8", { fatal: true }); // Without fatal=true it never fails

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
