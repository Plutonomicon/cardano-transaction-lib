import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

export const newPositive = lib.Int.new;
export const newNegative = lib.Int.new_negative;

export function _intToStr(n) {
  return n.to_str();
}
