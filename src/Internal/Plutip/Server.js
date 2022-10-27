const fs = require("fs").promises;

exports._removeDir = function (path) {
  return function () {
    return fs.rm(path, { recursive: true, force: true });
  };
};
