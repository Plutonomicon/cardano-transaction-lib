/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports._minFee = maybe => tx => txFeeFixed => txFeePerByte => {
  try {
    const linearFee = lib.LinearFee.new(txFeePerByte, txFeeFixed);
    return maybe.just(lib.min_fee(tx, linearFee));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._minScriptFee = exUnitPrices => tx =>
  lib.min_script_fee(tx, exUnitPrices);
