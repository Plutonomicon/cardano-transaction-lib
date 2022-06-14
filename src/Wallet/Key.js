/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@emurgo/cardano-serialization-lib-browser');
} else {
    lib = require('@emurgo/cardano-serialization-lib-nodejs');
}

exports._privateKeyFromNormalBytes = maybe => bytes => {
    try {
        return maybe.just(lib.PrivateKey.from_normal_bytes(bytes));
    } catch (_) {
        return maybe.nothing;
    }
};
