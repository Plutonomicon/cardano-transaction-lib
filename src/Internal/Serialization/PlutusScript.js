import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

export function newPlutusV1Script(bytes) {
  return lib.PlutusScript.new(bytes);
}

export function newPlutusV2Script(bytes) {
  return lib.PlutusScript.new_v2(bytes);
}

export function plutusScriptBytes(script) {
  return script.bytes();
}
