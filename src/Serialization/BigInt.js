/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports._BigInt_from_str = helper => str => {
  try {
    return helper.just(lib.BigInt.from_str(str));
  } catch (_) {
    return helper.nothing;
  }
};
