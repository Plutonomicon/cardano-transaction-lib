/* global require exports BROWSER_RUNTIME */
var lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const plutusDataAs = (what) => (helper) => (data) => {
  const res = data["as_" + what]();
  return res == null ? helper.nothing : helper.just(res);
};

// :: MaybeFfiHelper -> PlutusData -> PlutusList
exports._PlutusData_constr = plutusDataAs("constr_plutus_data");
// :: MaybeFfiHelper -> PlutusData -> PlutusMap
exports._PlutusData_map = plutusDataAs("map");
// :: MaybeFfiHelper -> PlutusData -> Maybe (Array PlutusData)
exports._PlutusData_list = plutusDataAs("list");
// :: MaybeFfiHelper -> PlutusData -> Maybe BigInt
exports._PlutusData_integer = plutusDataAs("integer");
// :: MaybeFfiHelper -> PlutusData -> Maybe ByteArray
exports._PlutusData_bytes = plutusDataAs("bytes");
exports._unpackPlutusList = (containerHelper) => containerHelper.unpack;
exports._ConstrPlutusData_alternative = (x) => x.alternative();
exports._ConstrPlutusData_data = (x) => x.data();

exports._unpackPlutusMap = (containerHelper) => (tuple) => (plutusMap) => {
  const keys = containerHelper.unpack(plutusMap.keys());
  const res = [];
  for (let key of keys) {
    // Assuming that `PlutusMap.get()` never fails on elements from result of its `.keys()` call.
    res.push(tuple(key)(plutusMap.get(key)));
  }
  return res;
};
