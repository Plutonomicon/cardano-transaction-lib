import os from "os";

export function tmpdir() {
  return os.tmpdir();
}

export function setLineHandler(interf) {
  return callback => () => {
    const signal = "line";
    const cb = line => callback(line)();
    interf.on(signal, cb);
    return { signal, callback: cb };
  };
}

export function setCloseHandler(readline) {
  return callback => () => {
    const signal = "close";
    readline.on(signal, callback);
    return { signal, callback };
  };
}
