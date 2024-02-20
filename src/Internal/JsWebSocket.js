import ReconnectingWebSocket from "reconnecting-websocket";
import WebSocket from "isomorphic-ws";

class NoPerMessageDeflateWebSocket extends WebSocket {
  constructor(url, protocols, options) {
    options = options || {};
    options.perMessageDeflate = false;
    super(url, protocols, options);
  }
}

export function _mkWebSocket(logger) {
  return url => () => {
    try {
      const ws = new ReconnectingWebSocket(url, [], {
        WebSocket: NoPerMessageDeflateWebSocket
      });
      ws.finalizers = [];
      logger("Created a new WebSocket")();
      return ws;
    } catch (e) {
      logger("Failed to create a new WebSocket");
      throw e;
    }
  };
}

export function _onWsConnect(ws) {
  return fn => () => {
    ws.addEventListener("open", fn);
    ws.finalizers.push(() => {
      ws.removeEventListener("open", fn);
    });
  };
}

export function _onWsError(ws) {
  return fn => () => {
    const listener = function (event) {
      if (
        "message" in event &&
        typeof event.message === "string" &&
        event.message.length > 0
      ) {
        fn(event.message)();
      } else if ("error" in event && event.error instanceof Error) {
        fn(event.error.toString())();
      } else {
        fn(event.toString())();
      }
    };
    ws.addEventListener("error", listener);
    ws.finalizers.push(() => {
      ws.removeEventListener("error", listener);
    });
    return listener;
  };
}

export function _removeOnWsError(ws) {
  return listener => () => ws.removeEventListener("error", listener);
}

export function _onWsMessage(ws) {
  return logger => fn => () => {
    const listener = function func(event) {
      const str = event.data;
      logger(`message: ${str}`)();
      fn(str)();
    };
    ws.addEventListener("message", listener);
    ws.finalizers.push(() => {
      ws.removeEventListener("message", listener);
    });
  };
}

export function _wsFinalize(ws) {
  return () => {
    for (let finalizer of ws.finalizers) {
      /* eslint-disable no-empty */
      try {
        finalizer();
      } catch (_) {}
      /* eslint-enable */
    }
    ws.finalizers = [];
  };
}

export function _wsSend(ws) {
  return logger => str => () => {
    logger(`sending: ${str}`)();
    ws.send(str);
  };
}

export function _wsReconnect(ws) {
  return () => {
    ws.reconnect();
  };
}

export function _wsClose(ws) {
  return () => {
    ws.close();
  };
}
