// NOTE: Adopted from https://github.com/AlexaDeWit/purescript-text-encoding/blob/master/src/Data/TextDecoding.js
'use strict';

function importTextEncoding() {
    try {
        return require('util').TextDecoder;
    } catch (e) {
        throw new Error('TextEncoder could not be imported from node environment: ' + e);
    }
};

// `TextDecoder` is not available in `node`, use polyfill in that case
var TextDecoder =
    (typeof window === 'object' && window.TextDecoder) ||
    (typeof require === 'function' && importTextEncoding());

exports._decodeUtf8 = buffer => left => right => {
    var decoder = new TextDecoder("utf-8", {fatal: true});

    try {
        return right(decoder.decode(buffer));
    } catch (err) {
        return left(err.toString());
    }
};

// FIXME: Move this to a better location
const call = property => object => object[property]();
exports.assetNameName = call('name');
