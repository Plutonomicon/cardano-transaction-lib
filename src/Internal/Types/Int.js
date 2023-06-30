/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

export const newPositive = lib.Int.new;
export const newNegative = lib.Int.new_negative;

export function _intToStr(n) {
  return n.to_str();
}
