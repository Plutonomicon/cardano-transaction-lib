/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export function _BigInt_from_str(helper) {
  return str => {
    try {
      return helper.just(lib.BigInt.from_str(str));
    } catch (_) {
      return helper.nothing;
    }
  };
}
