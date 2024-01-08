import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

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
