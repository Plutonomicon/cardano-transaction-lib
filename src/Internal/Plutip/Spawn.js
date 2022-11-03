"use strict";

exports.clearLineHandler = readline => () => {
  readline.removeAllListeners("line");
};

const fs = require("fs");

exports._rmdirSync = path => () => fs.rmdirSync(path, { recursive: true });
