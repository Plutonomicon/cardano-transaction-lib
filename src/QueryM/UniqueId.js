/* global require exports */

const uniqid = require("uniqid");

// _uniqueId :: String -> Effect String
exports.uniqueId = (str) => () => uniqid(str);
