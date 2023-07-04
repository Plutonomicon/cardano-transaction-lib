/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export function bnCompare(lhs) {
  return rhs => lhs.compare(rhs);
}

export const zero = lib.BigNum.zero();
export const one = lib.BigNum.one();

export function bnAdd(maybe) {
  return lhs => rhs => {
    try {
      return maybe.just(lhs.checked_add(rhs));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function bnMul(maybe) {
  return lhs => rhs => {
    try {
      return maybe.just(lhs.checked_mul(rhs));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function _fromString(maybe) {
  return str => {
    try {
      return maybe.just(lib.BigNum.from_str(str));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function toString(bn) {
  return bn.to_str();
}
