/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

export function newPlutusV1Script(bytes) {
  return lib.PlutusScript.new(bytes);
}

export function newPlutusV2Script(bytes) {
  return lib.PlutusScript.new_v2(bytes);
}

export function newPlutusV3Script(bytes) {
  return lib.PlutusScript.new_v3(bytes);
}

export function plutusScriptBytes(script) {
  return script.bytes();
}
