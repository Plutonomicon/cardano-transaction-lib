/* global BROWSER_RUNTIME */

let lib, csl;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-message-signing-browser");
  csl = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@emurgo/cardano-message-signing-nodejs");
  csl = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);
// csl = gcWrapper(csl);

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

const fromBytes = name => bytes => () => {
  return lib[name].from_bytes(bytes);
};

// -----------------------------------------------------------------------------
// PublicKey
// -----------------------------------------------------------------------------

// verifySignature :: COSESign1 -> PublicKey -> CborBytes -> Effect Boolean
export function verifySignature(coseSign1) {
  return publicKey => sigStructBytes => () => {
    const signature = csl.Ed25519Signature.from_bytes(coseSign1.signature());
    return publicKey.verify(sigStructBytes, signature);
  };
}

// -----------------------------------------------------------------------------
// COSESign1
// -----------------------------------------------------------------------------

// _fromBytesCoseSign1 :: CborBytes -> Effect COSESign1
export const fromBytesCoseSign1 = fromBytes("COSESign1");

// getSignedData :: COSESign1 -> Effect CborBytes
export function getSignedData(coseSign1) {
  return () => {
    return coseSign1.signed_data(null, null).to_bytes();
  };
}

// getCoseSign1ProtectedHeaders :: COSESign1 -> HeaderMap
const getCoseSign1ProtectedHeaders = coseSign1 => {
  return coseSign1.headers().protected().deserialized_headers();
};

// getCoseSign1ProtectedHeaderAlg :: MaybeFfiHelper -> COSESign1 -> Maybe Int
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

// _getCoseSign1ProtectedHeaderAddress
//   :: MaybeFfiHelper -> COSESign1 -> Maybe CborBytes
export function _getCoseSign1ProtectedHeaderAddress(maybe) {
  return coseSign1 => {
    const protectedHeaders = getCoseSign1ProtectedHeaders(coseSign1);
    const cborValue = protectedHeaders.header(lib.Label.new_text("address"));
    return opt_chain(maybe, cborValue, "as_bytes");
  };
}

// _getCoseSign1ProtectedHeaderKid
//   :: MaybeFfiHelper -> COSESign1 -> Maybe RawBytes
export function _getCoseSign1ProtectedHeaderKid(maybe) {
  return coseSign1 => {
    const protectedHeaders = getCoseSign1ProtectedHeaders(coseSign1);
    return opt_chain(maybe, protectedHeaders, "key_id");
  };
}

// -----------------------------------------------------------------------------
// COSEKey
// -----------------------------------------------------------------------------

// _fromBytesCoseKey :: CborBytes -> Effect COSEKey
export const fromBytesCoseKey = fromBytes("COSEKey");

// _getCoseKeyHeaderKty :: MaybeFfiHelper -> COSEKey -> Maybe Int
export function _getCoseKeyHeaderKty(maybe) {
  return coseKey => {
    return opt_chain(maybe, coseKey.key_type(), "as_int", "as_i32");
  };
}

// _getCoseKeyHeaderAlg :: MaybeFfiHelper -> COSEKey -> Maybe Int
export function _getCoseKeyHeaderAlg(maybe) {
  return coseKey => {
    return opt_chain(maybe, coseKey, "algorithm_id", "as_int", "as_i32");
  };
}

// _getCoseKeyHeaderCrv :: MaybeFfiHelper -> COSEKey -> Maybe Int
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

// _getCoseKeyHeaderX :: MaybeFfiHelper -> COSEKey -> Maybe RawBytes
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

// _getCoseKeyHeaderKid :: MaybeFfiHelper -> COSESign1 -> Maybe RawBytes
export function _getCoseKeyHeaderKid(maybe) {
  return coseKey => {
    return opt_chain(maybe, coseKey, "key_id");
  };
}
