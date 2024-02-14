import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

export function _BigInt_from_str(helper) {
  return str => {
    try {
      return helper.just(lib.BigInt.from_str(str));
    } catch (_) {
      return helper.nothing;
    }
  };
}
