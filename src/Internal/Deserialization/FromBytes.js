import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

export function _fromBytes(helper) {
  return name => bytes => {
    try {
      return helper.valid(lib[name].from_bytes(bytes));
    } catch (e) {
      return helper.error(name + ".from_bytes() raised " + e);
    }
  };
}
