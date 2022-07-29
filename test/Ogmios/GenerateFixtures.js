"use strict";

const crypto = require("crypto");

exports.md5 = function (message) {
  return crypto.createHash("md5").update(message).digest("hex");
};
