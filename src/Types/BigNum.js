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

const add = (a, b) => {
  return a + b;
};

const mul = (a, b) => {
  return a * b;
};

const check_limit_with_func = (a, b, limit, func) => {
  const r = func(a, b);
  if (r < limit) {
    return r;
  } else {
    throw new Error("Overflow detected");
  }
};

const check_limit = (str, limit) => {
  if (0 <= BigInt(str) && BigInt(str) < limit) {
    return BigInt(str);
  } else {
    throw new Error("Overflow detected");
  }
};

exports.bnAdd = maybe => lhs => rhs => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  try {
    check_limit_with_func(
      lhs.to_str(),
      rhs.to_str(),
      BigInt("18446744073709551616"),
      add
    ); // 2 ^ 64
    return maybe.just(lhs.checked_add(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.bnMul = maybe => lhs => rhs => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  try {
    check_limit_with_func(
      lhs.to_str(),
      rhs.to_str(),
      BigInt("18446744073709551616"),
      mul
    ); // 2 ^ 64
    return maybe.just(lhs.checked_mul(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._fromString = maybe => str => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  try {
    check_limit(str, BigInt("18446744073709551616")); //2 ^ 64
    return maybe.just(lib.BigNum.from_str(str));
  } catch (_) {
    return maybe.nothing;
  }
};

console.log(check_limit("22", 13));
console.log(check_limit_with_func(1, 2, BigInt("4"), add));

exports.toString = bn => bn.to_str();
