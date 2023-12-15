/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export function _minFee(maybe) {
  return tx => txFeeFixed => txFeePerByte => {
    try {
      const linearFee = lib.LinearFee.new(txFeePerByte, txFeeFixed);
      return maybe.just(lib.min_fee(tx, linearFee));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function _minScriptFee(exUnitPrices) {
  return tx => lib.min_script_fee(tx, exUnitPrices);
}
