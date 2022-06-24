/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
  lib = require('@emurgo/cardano-serialization-lib-browser');
} else {
  lib = require('@emurgo/cardano-serialization-lib-nodejs');
}

exports.bnCompare = lhs => rhs => lhs.compare(rhs);

exports.zero = lib.BigNum.zero();

exports.one = lib.BigNum.one();

exports.bnAdd = maybe => lhs => rhs => {
  try {
    return maybe.just(lhs.checked_add(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.bnMul = maybe => lhs => rhs => {
  try {
    return maybe.just(lhs.checked_mul(rhs));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._fromString = maybe => str => {
  try {
    return maybe.just(lib.BigNum.from_str(str));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.toString = bn => bn.to_str()
