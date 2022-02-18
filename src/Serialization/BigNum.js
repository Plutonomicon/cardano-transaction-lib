/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

exports.bigNumFromString = helper => str => {
    try {
        return helper.just(lib.BigNum.from_str(str));
    } catch (_) {
        return helper.nothing;
    }
};
