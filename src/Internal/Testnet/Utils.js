import os from "os";

try {
  process.setMaxListeners(10000);
} catch (e) {
  console.warn(e);
}

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
export function setErrorHandler(interfc) {
  return callback => () => {
    const signal = "error";
    const cb = err => callback(err)();
    interfc.on(signal, cb);
    return { signal, callback: cb };
  };
}
export function onExit(callback) {
  const cb = exitcode => {
    callback(exitcode)();
  };
  const signal = "exit";
  return () => {
    process.on(signal, cb);
    return { signal, callback };
  };
}
export function onBeforeExit(callback) {
  const signal = "beforeExit";
  return () => {
    process.on(signal, callback);
    return { signal, callback };
  };
}
export function onUncaughtException(callback) {
  const cb = error => {
    callback(error)();
  };
  const signal = "uncaughtException";
  return () => {
    process.on(signal, cb);
    return { signal, callback };
  };
}
export function setCloseHandler(readline) {
  return callback => () => {
    const signal = "close";
    readline.on(signal, callback);
    return { signal, callback };
  };
}
