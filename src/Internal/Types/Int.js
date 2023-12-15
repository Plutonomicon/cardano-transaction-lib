/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export const newPositive = lib.Int.new;
export const newNegative = lib.Int.new_negative;

export function _intToStr(n) {
  return n.to_str();
}
