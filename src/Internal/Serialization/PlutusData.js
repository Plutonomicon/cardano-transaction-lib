import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

export function _mkPlutusData_bytes(bytes) {
  return lib.PlutusData.new_bytes(bytes);
}

export function _mkPlutusData_list(list) {
  return lib.PlutusData.new_list(list);
}

export function _mkPlutusData_map(list) {
  return lib.PlutusData.new_map(list);
}

export function _mkPlutusData_integer(int) {
  return lib.PlutusData.new_integer(int);
}

export function _mkPlutusData_constr(constr) {
  return lib.PlutusData.new_constr_plutus_data(constr);
}

export function _packPlutusList(containerHelper) {
  return elems => containerHelper.pack(lib.PlutusList, elems);
}

export function _mkConstrPlutusData(n) {
  return list => lib.ConstrPlutusData.new(n, list);
}

export function _bigIntFromString(maybe) {
  return str => {
    try {
      return maybe.just(lib.BigInt.from_str(str));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function _packMap(first) {
  return second => kvs => {
    const res = lib.PlutusMap.new();
    for (let kv of kvs) {
      res.insert(first(kv), second(kv));
    }
    return res;
  };
}
