/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

exports._mkPlutusData_bytes = bytes => lib.PlutusData.new_bytes(bytes);
exports._mkPlutusData_list = list => lib.PlutusData.new_list(list);
exports._mkPlutusData_map = list => lib.PlutusData.new_map(list);
exports._mkPlutusData_integer = int => lib.PlutusData.new_integer(int);
exports._mkPlutusData_constr = constr => lib.PlutusData.new_constr_plutus_data(constr);

exports._packPlutusList = helper => elems => helper.pack(lib.PlutusList, elems);
exports._mkConstrPlutusData = n => list => lib.ConstrPlutusData.new(n, list);

exports._bigIntFromString = helper => str => {
    try {
        return helper.just(lib.BigInt.from_str(str));
    } catch (_) {
        return helper.nothing;
    }
};

exports._packMap = helper => first => second => kvs => {
    const res = lib.PlutusMap.new();
    for (let kv of kvs) {
        const success = res.insert(first(kv), second(kv));
        if (success == null) {
            return helper.nothing;
        }
    }
    return helper.just(res);
}
