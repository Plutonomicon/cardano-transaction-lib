/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports._BigInt_from_str = maybe => str => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  if (/^-?[0-9]+$/.test(str)) {
    return maybe.just(lib.BigInt.from_str(str));
  } else {
    return maybe.nothing;
  }
};
