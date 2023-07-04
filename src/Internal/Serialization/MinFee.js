/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

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
