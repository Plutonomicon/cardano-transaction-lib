exports._showUint8Array = bytes =>
    Array.prototype.map.bind(bytes)(
        byte =>
            byte.toString(16).padStart(2,'0')).join('');

exports._emptyUint8Array = new Uint8Array();

exports._eqUint8Array = bytes1 => bytes2 =>
    bytes1 === bytes2;

exports._byteLengthUint8Array = bytes =>
    Uint8Array.prototype.byteLength(bytes)