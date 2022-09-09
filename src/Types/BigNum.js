/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports.bnCompare = lhs => rhs => lhs.compare(rhs);

exports.zero = lib.BigNum.zero();

exports.one = lib.BigNum.one();

exports.bnAdd = maybe => lhs => rhs => {
  const check_add = (a, b, limit) => {
    const r = a + b;
    if (r < limit) {
      return r;
    } else {
      throw new Error("Overflow detected");
    }
  };
  try {
    check_add(lhs.to_str(), rhs.to_str(), BitInt("18446744073709551616")); // 2 ^ 64
    return maybe.just(lhs.checked_add(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.bnMul = maybe => lhs => rhs => {
  const check_mul = (a, b, limit) => {
    const r = a * b;
    if (r < limit) {
      return r;
    } else {
      throw new Error("Overflow detected");
    }
  };

  try {
    check_mul(lhs.to_str(), rhs.to_str(), BigInt("18446744073709551616")); // 2 ^ 64
    return maybe.just(lhs.checked_mul(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._fromString = maybe => str => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  if (str[0] == "-") {
    return maybe.nothing;
  }
  try {
    return maybe.just(lib.BigNum.from_str(str));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.toString = bn => bn.to_str();
