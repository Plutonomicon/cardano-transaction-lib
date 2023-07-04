/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export function _fromBytes(helper) {
  return name => bytes => {
    try {
      return helper.valid(lib[name].from_bytes(bytes));
    } catch (e) {
      return helper.error(name + ".from_bytes() raised " + e);
    }
  };
}
