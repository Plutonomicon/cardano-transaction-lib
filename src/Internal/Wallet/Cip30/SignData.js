/* global BROWSER_RUNTIME */

let lib, csl;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-message-signing-asmjs");
  csl = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-message-signing-nodejs");
  csl = require("@emurgo/cardano-serialization-lib-nodejs");
}

// newCoseSign1Builder :: ByteArray -> Headers -> COSESign1Builder
exports.newCoseSign1Builder = payload => headers => {
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

// newHeaders :: HeaderMap -> ProtectedHeaderMap -> Headers
exports.newHeaders = unprotectedHeaders => protectedHeaders => {
  return lib.Headers.new(protectedHeaders, unprotectedHeaders);
};

// newHeaderMap :: HeaderMap
exports.newHeaderMap = lib.HeaderMap.new();

// newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap
exports.newProtectedHeaderMap = headerMap => {
  return lib.ProtectedHeaderMap.new(headerMap);
};

// setAlgHeaderToEdDsa :: HeaderMap -> HeaderMap
exports.setAlgHeaderToEdDsa = headerMap => {
  const label = lib.Label.from_algorithm_id(lib.AlgorithmId.EdDSA);
  headerMap.set_algorithm_id(label);
  return headerMap;
};

// setAddressHeader :: ByteArray -> HeaderMap -> HeaderMap
exports.setAddressHeader = addressBytes => headerMap => {
  const label = lib.Label.new_text("address");
  const value = lib.CBORValue.new_bytes(addressBytes);
  headerMap.set_header(label, value);
  return headerMap;
};
