/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

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
