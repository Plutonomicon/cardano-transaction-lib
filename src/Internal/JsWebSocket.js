/* global BROWSER_RUNTIME */

const ReconnectingWebSocket = require("reconnecting-websocket");

let OurWebSocket;
if (typeof BROWSER_RUNTIME == "undefined" || !BROWSER_RUNTIME) {
  OurWebSocket = require("ws");
} else {
  OurWebSocket = WebSocket;
}

class NoPerMessageDeflateWebSocket extends OurWebSocket {
  constructor(url, protocols, options) {
    options = options || {};
    options.perMessageDeflate = false;
    super(url, protocols, options);
  }
}

exports._mkWebSocket = logger => url => () => {
  try {
    let ws;
    if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
      ws = new ReconnectingWebSocket.default(url);
    } else {
      ws = new ReconnectingWebSocket(url, [], {
        WebSocket: NoPerMessageDeflateWebSocket,
      });
    }
    ws.finalizers = [];
    logger("Created a new WebSocket")();
    return ws;
  } catch (e) {
    logger("Failed to create a new WebSocket");
    throw e;
  }
};

exports._onWsConnect = ws => fn => () => {
  ws.addEventListener("open", fn);
  ws.finalizers.push(() => {
    ws.removeEventListener("open", fn);
  });
};

exports._onWsError = ws => fn => () => {
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

exports._removeOnWsError = ws => listener => () =>
  ws.removeEventListener("error", listener);

exports._onWsMessage = ws => logger => fn => () => {
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

exports._wsFinalize = ws => () => {
  for (let finalizer of ws.finalizers) {
    /* eslint-disable no-empty */
    try {
      finalizer();
    } catch (_) {}
    /* eslint-enable */
  }
  ws.finalizers = [];
};

exports._wsSend = ws => logger => str => () => {
  logger(`sending: ${str}`)();
  ws.send(str);
};

exports._wsReconnect = ws => () => {
  ws.reconnect();
};

exports._wsClose = ws => () => {
  ws.close();
};
