"use strict";

exports.clearLineHandler = readline => () => {
  readline.removeAllListeners("line");
};

const fs = require("fs");

exports._rmdirSync = path => () => fs.rmdirSync(path, { recursive: true });

exports.removeOnSignal =
  ({ signal, callback }) =>
  () => {
    process.removeListener(signal, callback);
  };

exports.onSignalImpl = signal => callback => () => {
  process.on(signal, callback);
  return { signal, callback };
};
