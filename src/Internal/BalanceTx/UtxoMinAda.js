/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

export function minAdaForOutput(maybe) {
  return txOutput => dataCost => {
    try {
      return maybe.just(lib.min_ada_for_output(txOutput, dataCost));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function newCoinsPerWord(n) {
  return lib.DataCost.new_coins_per_word(n);
}

export function newCoinsPerByte(n) {
  return lib.DataCost.new_coins_per_byte(n);
}
