exports.showUint8Array = bytes =>
    Array.prototype.map.bind(bytes)(
        byte =>
            byte.toString(16).padStart(2,'0')).join('');
