const uniqid = require("uniqid");

exports.uniqueId = str => () => uniqid(str);
