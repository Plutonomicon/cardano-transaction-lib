/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

export function newPlutusV1Script(bytes) {
  return lib.PlutusScript.new(bytes);
}

export function newPlutusV2Script(bytes) {
  return lib.PlutusScript.new_v2(bytes);
}

export function plutusScriptBytes(script) {
  return script.bytes();
}
