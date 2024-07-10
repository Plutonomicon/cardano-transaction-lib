"use strict";

import stream from "node:stream";
import net from "net";

export function clearLineHandler(readline) {
  return () => {
    readline.removeAllListeners("line");
  };
}

export const readableFromBuffer = buf => () =>
  stream.Readable.from(buf, { objectMode: false });

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

export function _isPortAvailable(port) {
  return () =>
    new Promise((resolve, reject) => {
      const server = net
        .createServer()
        .once("error", function (err) {
          if (err.code == "EADDRINUSE") {
            resolve(false);
          } else {
            reject(
              "Failed check for port availability (port: " +
                port +
                ", error: " +
                err.code +
                ")"
            );
          }
        })
        .once("listening", () => {
          server.once("close", () => resolve(true)).close();
        })
        .listen(port);
    });
}
