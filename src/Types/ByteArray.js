// From instance for Array in prelude
exports.ordImpl = function (f) {
    return function (xs) {
        return function (ys) {
            var i = 0;
            var xlen = xs.length;
            var ylen = ys.length;
            while (i < xlen && i < ylen) {
                var x = xs[i];
                var y = ys[i];
                var o = f(x)(y);
                if (o !== 0) {
                    return o;
                }
                i++;
            }
            if (xlen === ylen) {
                return 0;
            } else if (xlen > ylen) {
                return -1;
            } else {
                return 1;
            }
        };
    };
};

exports.concatImpl = function (xs) {
    return function (ys) {
        const res = new Uint8Array(xs.length + ys.length);
        for (let i = 0; i < xs.length; i++) {
            res[i] = xs[i];
        }
        for (let i = 0; i < ys.length; i++) {
            res[i + xs.length] = ys[i];
        }
        return res;
    };
};

exports.byteArrayToHex = arr =>
    Buffer.from(arr).toString('hex');

exports.hexToByteArrayImpl = nothing => just => hex => {
    for (var bytes = [], c = 0; c < hex.length; c += 2) {
        let chunk = hex.substr(c, 2);
        if (/[0-9a-f]{2}/i.test(chunk)) {
            bytes.push(parseInt(chunk, 16));
        } else {
            return nothing;
        }
    }
    return just(new Uint8Array(bytes));
};

exports.hexToByteArrayUnsafe = hex => {
    for (var bytes = [], c = 0; c < hex.length; c += 2)
        bytes.push(parseInt(hex.substr(c, 2), 16));
    return new Uint8Array(bytes);
};

exports.byteArrayFromIntArray = ints => new Uint8Array(ints);

exports.byteArrayToIntArray = bytes => Array.from(bytes);

// _byteLength :: Uint8Array -> BigInt
exports._byteLength = bytes =>
  Uint8Array.prototype.byteLength(bytes);
