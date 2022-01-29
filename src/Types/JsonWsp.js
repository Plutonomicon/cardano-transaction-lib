const uniqid = require ("uniqid");

// _uniqueId :: String -> Effect String
exports._uniqueId = str => () => uniqid(str)
