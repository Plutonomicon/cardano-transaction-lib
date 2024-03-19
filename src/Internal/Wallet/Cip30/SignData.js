import * as lib from "@mlabs-haskell/cardano-message-signing";

// -----------------------------------------------------------------------------
// COSESign1Builder
// -----------------------------------------------------------------------------

// newCoseSign1Builder :: ByteArray -> Headers -> Effect COSESign1Builder
export function newCoseSign1Builder(payload) {
  return headers => () => {
    return lib.COSESign1Builder.new(headers, payload, false);
  };
}

// makeDataToSign :: COSESign1Builder -> ByteArray
export function makeDataToSign(builder) {
  return builder.make_data_to_sign().to_bytes();
}

// sign :: PrivateKey -> ByteArray -> ByteArray
export function sign(privateKey) {
  return message => {
    return privateKey.sign(message).to_bytes();
  };
}

// buildSignature :: COSESign1Builder -> ByteArray -> ByteArray
export function buildSignature(builder) {
  return signedSigStruct => {
    return builder.build(signedSigStruct).to_bytes();
  };
}

// -----------------------------------------------------------------------------
// Headers
// -----------------------------------------------------------------------------

// newHeaders :: HeaderMap -> ProtectedHeaderMap -> Headers
export function newHeaders(unprotectedHeaders) {
  return protectedHeaders => {
    return lib.Headers.new(protectedHeaders, unprotectedHeaders);
  };
}

// -----------------------------------------------------------------------------
// ProtectedHeaderMap
// -----------------------------------------------------------------------------

// newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap
export function newProtectedHeaderMap(headerMap) {
  return lib.ProtectedHeaderMap.new(headerMap);
}

// -----------------------------------------------------------------------------
// HeaderMap
// -----------------------------------------------------------------------------

// newHeaderMap :: Effect HeaderMap
export function newHeaderMap() {
  return lib.HeaderMap.new();
}

// setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
export function setAlgHeaderToEdDsa(headerMap) {
  return () => {
    const label = lib.Label.from_algorithm_id(lib.AlgorithmId.EdDSA);
    headerMap.set_algorithm_id(label);
  };
}

// setAddressHeader :: ByteArray -> HeaderMap -> Effect Unit
export function setAddressHeader(addressBytes) {
  return headerMap => () => {
    const label = lib.Label.new_text("address");
    const value = lib.CBORValue.new_bytes(addressBytes);
    headerMap.set_header(label, value);
  };
}

// -----------------------------------------------------------------------------
// COSEKey
// -----------------------------------------------------------------------------

// newCoseKeyWithOkpType :: Effect COSEKey
export function newCoseKeyWithOkpType() {
  return lib.COSEKey.new(lib.Label.from_key_type(lib.KeyType.OKP));
}

// setCoseKeyAlgHeaderToEdDsa :: COSEKey -> Effect Unit
export function setCoseKeyAlgHeaderToEdDsa(key) {
  return () => {
    key.set_algorithm_id(lib.Label.from_algorithm_id(lib.AlgorithmId.EdDSA));
  };
}

// setCoseKeyCrvHeaderToEd25519 :: COSEKey -> Effect Unit
export function setCoseKeyCrvHeaderToEd25519(key) {
  return () => {
    key.set_header(
      lib.Label.new_int(
        lib.Int.new_negative(lib.BigNum.from_str("1")) // crv (-1)
      ),
      lib.CBORValue.new_int(
        lib.Int.new_i32(6) // Ed25519 (6)
      )
    );
  };
}

// setCoseKeyXHeader :: RawBytes -> COSEKey -> Effect Unit
export function setCoseKeyXHeader(publicKeyBytes) {
  return key => () => {
    key.set_header(
      lib.Label.new_int(
        lib.Int.new_negative(lib.BigNum.from_str("2")) // x (-2)
      ),
      lib.CBORValue.new_bytes(publicKeyBytes) // public key bytes
    );
  };
}

// bytesFromCoseKey :: COSEKey -> CborBytes
export function bytesFromCoseKey(key) {
  return key.to_bytes();
}
