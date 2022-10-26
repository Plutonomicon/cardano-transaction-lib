/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-message-signing-asmjs");
} else {
  lib = require("@emurgo/cardano-message-signing-nodejs");
}

// newCoseSign1Builder :: Headers -> Vec<u8> -> COSESign1Builder
exports.newCoseSign1Builder = headers => payload => {
  return lib.COSESign1Builder.new(headers, payload, false);
};

// newHeaders :: ProtectedHeaderMap -> HeaderMap -> Headers
exports.newHeaders = protectedHeaders => unprotectedHeaders => {
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

