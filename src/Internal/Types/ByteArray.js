// From instance for Array in prelude
export function ord_(f) {
  return xs => ys => {
    let i = 0;
    const xlen = xs.length;
    const ylen = ys.length;
    while (i < xlen && i < ylen) {
      let o = f(xs[i])(ys[i]);
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
}

export function concat_(xs) {
  return ys => {
    const res = new Uint8Array(xs.length + ys.length);
    for (let i = 0; i < xs.length; i++) {
      res[i] = xs[i];
    }
    for (let i = 0; i < ys.length; i++) {
      res[i + xs.length] = ys[i];
    }
    return res;
  };
}

export function byteArrayToHex(arr) {
  return Buffer.from(arr).toString("hex");
}

/* adapted from https://github.com/WebReflection/uint8-to-utf16/blob/master/esm/index.js
 * (someone who knows javascript should like import that or something)
 */
const { ceil } = Math;
const { fromCharCode } = String;

export function byteArrayToUTF16le(uint8array) {
  let extra = 0;
  const output = [];
  const { length } = uint8array;
  const len = ceil(length / 2);
  for (let j = 0, i = 0; i < len; i++)
    output.push(
      fromCharCode(
        (uint8array[j++] << 8) + (j < length ? uint8array[j++] : extra++)
      )
    );
  output.push(fromCharCode(extra));
  return output.join("");
}

export function hexToByteArray_(nothing) {
  return just => hex => {
    let bytes = [];
    for (let c = 0; c < hex.length; c += 2) {
      const chunk = hex.substr(c, 2);
      if (/[0-9a-f]{2}/i.test(chunk)) {
        bytes.push(parseInt(chunk, 16));
      } else {
        return nothing;
      }
    }
    return just(new Uint8Array(bytes));
  };
}

export function hexToByteArrayUnsafe(hex) {
  let bytes = [];
  for (let c = 0; c < hex.length; c += 2) {
    bytes.push(parseInt(hex.substr(c, 2), 16));
  }
  return new Uint8Array(bytes);
}

export function byteArrayFromIntArrayUnsafe(ints) {
  return new Uint8Array(ints);
}

export function byteArrayFromInt16ArrayUnsafe(ints) {
  return new Uint8Array(ints.buffer, ints.byteOffset, ints.byteLength);
}

export function byteArrayFromIntArray_(nothing) {
  return just => ints => {
    if (ints.every(i => i < 256 && i >= 0)) {
      return just(new Uint8Array(ints));
    } else {
      return nothing;
    }
  };
}

export function byteArrayToIntArray(bytes) {
  return Array.from(bytes);
}

export function byteLength(bytes) {
  return bytes.byteLength;
}

export function subarray(start) {
  return end => bytes => bytes.subarray(start, end);
}
