/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require('@mlabs-haskell/csl-gc-wrapper')(lib)

exports.newPositive = lib.Int.new;
exports.newNegative = lib.Int.new_negative;
exports._intToStr = n => n.to_str();
