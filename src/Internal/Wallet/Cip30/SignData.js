/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-message-signing-browser");
} else {
  lib = require("@emurgo/cardano-message-signing-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

// -----------------------------------------------------------------------------
// COSESign1Builder
// -----------------------------------------------------------------------------

// newCoseSign1Builder :: ByteArray -> Headers -> Effect COSESign1Builder
exports.newCoseSign1Builder = payload => headers => () => {
  return lib.COSESign1Builder.new(headers, payload, false);
};

// makeDataToSign :: COSESign1Builder -> ByteArray
exports.makeDataToSign = builder => {
  return builder.make_data_to_sign().to_bytes();
};

// sign :: PrivateKey -> ByteArray -> ByteArray
exports.sign = privateKey => message => {
  return privateKey.sign(message).to_bytes();
};

// buildSignature :: COSESign1Builder -> ByteArray -> ByteArray
exports.buildSignature = builder => signedSigStruct => {
  return builder.build(signedSigStruct).to_bytes();
};

// -----------------------------------------------------------------------------
// Headers
// -----------------------------------------------------------------------------

// newHeaders :: HeaderMap -> ProtectedHeaderMap -> Headers
exports.newHeaders = unprotectedHeaders => protectedHeaders => {
  return lib.Headers.new(protectedHeaders, unprotectedHeaders);
};

// -----------------------------------------------------------------------------
// ProtectedHeaderMap
// -----------------------------------------------------------------------------

// newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap
exports.newProtectedHeaderMap = headerMap => {
  return lib.ProtectedHeaderMap.new(headerMap);
};

// -----------------------------------------------------------------------------
// HeaderMap
// -----------------------------------------------------------------------------

// newHeaderMap :: Effect HeaderMap
exports.newHeaderMap = () => {
  return lib.HeaderMap.new();
};

// setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
exports.setAlgHeaderToEdDsa = headerMap => () => {
  const label = lib.Label.from_algorithm_id(lib.AlgorithmId.EdDSA);
  headerMap.set_algorithm_id(label);
};

// setAddressHeader :: ByteArray -> HeaderMap -> Effect Unit
exports.setAddressHeader = addressBytes => headerMap => () => {
  const label = lib.Label.new_text("address");
  const value = lib.CBORValue.new_bytes(addressBytes);
  headerMap.set_header(label, value);
};

// -----------------------------------------------------------------------------
// COSEKey
// -----------------------------------------------------------------------------

// newCoseKeyWithOkpType :: Effect COSEKey
exports.newCoseKeyWithOkpType = () => {
  return lib.COSEKey.new(lib.Label.from_key_type(lib.KeyType.OKP));
};

// setCoseKeyAlgHeaderToEdDsa :: COSEKey -> Effect Unit
exports.setCoseKeyAlgHeaderToEdDsa = key => () => {
  key.set_algorithm_id(lib.Label.from_algorithm_id(lib.AlgorithmId.EdDSA));
};

// setCoseKeyCrvHeaderToEd25519 :: COSEKey -> Effect Unit
exports.setCoseKeyCrvHeaderToEd25519 = key => () => {
  key.set_header(
    lib.Label.new_int(
      lib.Int.new_negative(lib.BigNum.from_str("1")) // crv (-1)
    ),
    lib.CBORValue.new_int(
      lib.Int.new_i32(6) // Ed25519 (6)
    )
  );
};

// setCoseKeyXHeader :: RawBytes -> COSEKey -> Effect Unit
exports.setCoseKeyXHeader = publicKeyBytes => key => () => {
  key.set_header(
    lib.Label.new_int(
      lib.Int.new_negative(lib.BigNum.from_str("2")) // x (-2)
    ),
    lib.CBORValue.new_bytes(publicKeyBytes) // public key bytes
  );
};

// bytesFromCoseKey :: COSEKey -> CborBytes
exports.bytesFromCoseKey = key => key.to_bytes();
