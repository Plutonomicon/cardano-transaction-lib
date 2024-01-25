"use strict";

export function clearLineHandler(readline) {
  return () => {
    readline.removeAllListeners("line");
  };
}

import fs from "fs";

export function _rmdirSync(path) {
  return () => fs.rmSync(path, { recursive: true });
}

export function removeOnSignal({ signal, callback }) {
  return () => {
    process.removeListener(signal, callback);
  };
}

export function onSignalImpl(signal) {
  return callback => () => {
    process.on(signal, callback);
    return { signal, callback };
  };
}
