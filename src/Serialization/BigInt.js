/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const checkStr = str => {
  if (BigInt(str)) {
    return str;
  } else {
    throw new Error("Not a number");
  }
};

exports._BigInt_from_str = helper => str => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  try {
    checkStr(str);
    return helper.just(lib.BigInt.from_str(str));
  } catch (_) {
    return helper.nothing;
  }
};
