const uniqid = require ("uniqid");

// _uniqueId :: String -> Effect String
exports._uniqueId = str => () => uniqid(str)

exports.emptyUint8Array = new Uint8Array();
