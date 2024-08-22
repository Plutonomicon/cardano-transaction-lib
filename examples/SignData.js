"use strict";

// eslint-disable-next-line no-unused-vars
import * as lib from "@mlabs-haskell/cardano-message-signing";
import * as CSL from "@mlabs-haskell/cardano-serialization-lib-gc";

function opt_chain(maybe, obj) {
  const isNothing = x => x === null || x === undefined;
  let result = obj;
  for (let i = 2; i < arguments.length; i++) {
    if (isNothing(result)) {
      return maybe.nothing;
    } else {
      result = result[arguments[i]]();
    }
  }
  return isNothing(result) ? maybe.nothing : maybe.just(result);
}

const getCoseSign1ProtectedHeaders = coseSign1 => {
  return coseSign1.headers().protected().deserialized_headers();
};

export function _getCoseSign1ProtectedHeaderAlg(maybe) {
  return coseSign1 => {
    const protectedHeaders = getCoseSign1ProtectedHeaders(coseSign1);
    return opt_chain(
      maybe,
      protectedHeaders,
      "algorithm_id",
      "as_int",
      "as_i32"
    );
  };
}

export function _getCoseSign1ProtectedHeaderAddress(maybe) {
  return coseSign1 => {
    const protectedHeaders = getCoseSign1ProtectedHeaders(coseSign1);
    const cborValue = protectedHeaders.header(lib.Label.new_text("address"));
    return opt_chain(maybe, cborValue, "as_bytes");
  };
}

export function _getCoseKeyHeaderKty(maybe) {
  return coseKey => {
    return opt_chain(maybe, coseKey.key_type(), "as_int", "as_i32");
  };
}

export function _getCoseKeyHeaderAlg(maybe) {
  return coseKey => {
    return opt_chain(maybe, coseKey, "algorithm_id", "as_int", "as_i32");
  };
}

export function _getCoseKeyHeaderCrv(maybe) {
  return coseKey => {
    const cborValue = coseKey.header(
      lib.Label.new_int(
        lib.Int.new_negative(lib.BigNum.from_str("1")) // crv (-1)
      )
    );
    return opt_chain(maybe, cborValue, "as_int", "as_i32");
  };
}

export function _getCoseSign1ProtectedHeaderKid(maybe) {
  return coseSign1 => {
    const protectedHeaders = getCoseSign1ProtectedHeaders(coseSign1);
    return opt_chain(maybe, protectedHeaders, "key_id");
  };
}

export function _getCoseKeyHeaderKid(maybe) {
  return coseKey => {
    return opt_chain(maybe, coseKey, "key_id");
  };
}
export function _getCoseKeyHeaderX(maybe) {
  return coseKey => {
    const cborValue = coseKey.header(
      lib.Label.new_int(
        lib.Int.new_negative(lib.BigNum.from_str("2")) // x (-2)
      )
    );
    return opt_chain(maybe, cborValue, "as_bytes");
  };
}
export function getSignedData(coseSign1) {
  return () => {
    return coseSign1.signed_data(null, null).to_bytes();
  };
}
export function verifySignature(coseSign1) {
  return publicKey => sigStructBytes => () => {
    const signature = CSL.Ed25519Signature.from_bytes(coseSign1.signature());
    return publicKey.verify(sigStructBytes, signature);
  };
}
const fromBytes = name => bytes => () => {
  return lib[name].from_bytes(bytes);
};
export const fromBytesCoseKey = fromBytes("COSEKey");
export const fromBytesCoseSign1 = fromBytes("COSESign1");
