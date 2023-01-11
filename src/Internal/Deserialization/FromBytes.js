/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@mlabs-haskell/cardano-serialization-lib-browser");
} else {
  lib = require("@mlabs-haskell/cardano-serialization-lib-nodejs");
}

exports._fromBytes = helper => name => bytes => {
  try {
    return helper.valid(lib[name].from_bytes(bytes));
  } catch (e) {
    return helper.error(name + ".from_bytes() raised " + e);
  }
};
