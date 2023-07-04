/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

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
