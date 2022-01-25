// _byteLengthUint8Array :: Uint8Array -> BigInt
exports._byteLengthUint8Array = bytes =>
  Uint8Array.prototype.byteLength(bytes);

// _emptyUint8Array :: Uint8Array
exports._emptyUint8Array = new Uint8Array();

// _eqUint8Array :: Uint8Array -> Uint8Array -> Boolean
exports._eqUint8Array = bytes1 => bytes2 =>
  bytes1 === bytes2;

// _gtUint8Array :: Uint8Array -> Uint8Array -> Boolean
exports._gtUint8Array = bytes1 => bytes2 =>
  bytes1 > bytes2;

// _ltUint8Array :: Uint8Array -> Uint8Array -> Boolean
exports._ltUint8Array = bytes1 => bytes2 =>
  bytes1 < bytes2;

// _showUint8Array :: Uint8Array -> String
exports._showUint8Array = bytes =>
  Array.prototype.map.bind(bytes)(
    byte =>
      byte.toString(16).padStart(2,'0')).join('');
