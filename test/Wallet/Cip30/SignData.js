/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-message-signing-asmjs");
} else {
  lib = require("@emurgo/cardano-message-signing-nodejs");
}

const fromBytes = name => helper => bytes => {
  try {
    return helper.just(lib[name].from_bytes(bytes));
  } catch (_) {
    return helper.nothing;
  }
};

// _fromBytesCoseSign1 :: MaybeFfiHelper -> CborBytes -> Maybe COSESign1
exports._fromBytesCoseSign1 = fromBytes("COSESign1");

// _fromBytesCoseKey :: MaybeFfiHelper -> CborBytes -> Maybe COSEKey
exports._fromBytesCoseKey = fromBytes("COSEKey");

// getCoseSign1ProtectedHeaders :: COSESign1 -> HeaderMap
const getCoseSign1ProtectedHeaders = coseSign1 => {
  return coseSign1.headers().protected().deserialized_headers();
};

// getCoseSign1ProtectedHeaderAlg :: MaybeFfiHelper -> COSESign1 -> Maybe Int
exports._getCoseSign1ProtectedHeaderAlg = maybe => coseSign1 => {
  const protectedHeaders = getCoseSign1ProtectedHeaders(coseSign1);
  try {
    const value = protectedHeaders.algorithm_id().as_int().as_i32();
    return value != null ? maybe.just(value) : maybe.nothing;
  } catch (_) {
    return maybe.nothing;
  }
};

// _getCoseSign1ProtectedHeaderAddress
//   :: MaybeFfiHelper -> COSESign1 -> Maybe CborBytes
exports._getCoseSign1ProtectedHeaderAddress = maybe => coseSign1 => {
  const protectedHeaders = getCoseSign1ProtectedHeaders(coseSign1);
  try {
    const addressLabel = lib.Label.new_text("address");
    const value = protectedHeaders.header(addressLabel).as_bytes();
    return value != null ? maybe.just(value) : maybe.nothing;
  } catch (_) {
    return maybe.nothing;
  }
};
