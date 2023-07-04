/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export function newPlutusV1Script(bytes) {
  return lib.PlutusScript.new(bytes);
}

export function newPlutusV2Script(bytes) {
  return lib.PlutusScript.new_v2(bytes);
}

export function plutusScriptBytes(script) {
  return script.bytes();
}
