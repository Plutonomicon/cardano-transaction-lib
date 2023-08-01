/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export function _fromBytes(helper) {
  return name => bytes => {
    try {
      return helper.valid(lib[name].from_bytes(bytes));
    } catch (e) {
      return helper.error(name + ".from_bytes() raised " + e);
    }
  };
}
