// From instance for Array in prelude
exports.ord_ = f => xs => ys => {
    let i = 0;
    const xlen = xs.length;
    const ylen = ys.length;
    while (i < xlen && i < ylen) {
        var o = f(xs[i])(ys[i]);
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

exports.concat_ = xs => ys => {
    const res = new Uint8Array(xs.length + ys.length);
    for (let i = 0; i < xs.length; i++) {
        res[i] = xs[i];
    }
    for (let i = 0; i < ys.length; i++) {
        res[i + xs.length] = ys[i];
    }
    return res;
};

exports.byteArrayToHex = arr =>
    Buffer.from(arr).toString('hex');

exports.hexToByteArray_ = nothing => just => hex => {
    for (var bytes = [], c = 0; c < hex.length; c += 2) {
        const chunk = hex.substr(c, 2);
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

exports.byteArrayFromIntArrayUnsafe = ints => new Uint8Array(ints);

exports.byteArrayFromInt16ArrayUnsafe = ints => new Uint8Array(ints.buffer, ints.byteOffset, ints.byteLength);

exports.byteArrayFromIntArray_ = nothing => just => ints => {
    if (ints.every(i => i < 256 && i >= 0)) {
        return just(new Uint8Array(ints));
    } else {
        return nothing;
    }
};

exports.byteArrayToIntArray = bytes => Array.from(bytes);

// _byteLength :: Uint8Array -> BigInt
exports._byteLength = bytes =>
  bytes.byteLength;
