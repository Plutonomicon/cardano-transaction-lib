/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export const newPositive = lib.Int.new;
export const newNegative = lib.Int.new_negative;

export function _intToStr(n) {
  return n.to_str();
}
