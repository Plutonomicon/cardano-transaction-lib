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

const add = (a, b) => a + b;

const mul = (a, b) => a * b;

const bigNumLimit = BigInt("18446744073709551616"); // 2 ^ 64

const checkLimitWithFunc = (a, b, limit, func) => {
  const r = func(a, b);
  if (r < limit) {
    return r;
  } else {
    throw new Error("Overflow detected");
  }
};

const checkLimit = (str, limit) => {
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
    checkLimitWithFunc(
      BigInt(lhs.to_str()),
      BigInt(rhs.to_str()),
      bigNumLimit,
      add
    );
    return maybe.just(lhs.checked_add(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.bnMul = maybe => lhs => rhs => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  try {
    checkLimitWithFunc(
      BigInt(lhs.to_str()),
      BigInt(rhs.to_str()),
      bigNumLimit,
      mul
    );
    return maybe.just(lhs.checked_mul(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._fromString = maybe => str => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  try {
    checkLimit(str, bigNumLimit);
    return maybe.just(lib.BigNum.from_str(str));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.toString = bn => bn.to_str();
