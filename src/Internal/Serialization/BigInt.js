/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export function _BigInt_from_str(helper) {
  return str => {
    try {
      return helper.just(lib.BigInt.from_str(str));
    } catch (_) {
      return helper.nothing;
    }
  };
}
