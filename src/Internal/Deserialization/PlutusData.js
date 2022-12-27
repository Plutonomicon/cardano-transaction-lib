/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports._convertPlutusData = handle => pd => {
  switch (pd.kind()) {
    case lib.PlutusDataKind.ConstrPlutusData:
      return handle.constr(pd.as_constr_plutus_data());
    case lib.PlutusDataKind.Map:
      return handle.map(pd.as_map());
    case lib.PlutusDataKind.List:
      return handle.list(pd.as_list());
    case lib.PlutusDataKind.Integer:
      return handle.integer(pd.as_integer());
    case lib.PlutusDataKind.Bytes:
      return handle.bytes(pd.as_bytes());
    default:
      throw "Impossible PlutusData kind: " + pd.kind();
  }
};

exports._unpackPlutusList = containerHelper => containerHelper.unpack;
exports._ConstrPlutusData_alternative = x => x.alternative();
exports._ConstrPlutusData_data = x => x.data();

exports._unpackPlutusMap = containerHelper => tuple => plutusMap => {
  const keys = containerHelper.unpack(plutusMap.keys());
  const res = [];
  for (let key of keys) {
    // Assuming that `PlutusMap.get()` never fails on elements from result of
    // its `.keys()` call.
    res.push(tuple(key)(plutusMap.get(key)));
  }
  return res;
};
