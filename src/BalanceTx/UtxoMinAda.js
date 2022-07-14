/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports.minAdaForOutput = maybe => txOutput => coinsPerUtxoByte => {
  const dataCost = lib.DataCost.new_coins_per_byte(coinsPerUtxoByte);
  try {
    return maybe.just(lib.min_ada_for_output(txOutput, dataCost));
  } catch (_) {
    return maybe.nothing;
  }
};
