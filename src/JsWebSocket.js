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
    logger("Created a new WebSocket")();
    return ws;
  } catch (e) {
    logger("Failed to create a new WebSocket");
    throw e;
  }
};

exports._onWsConnect = ws => fn => () => ws.addEventListener("open", fn);

exports._onWsError = ws => fn => () => {
  const listener = function (event) {
    fn(event.toString())();
  };
  ws.addEventListener("error", listener);
  return listener;
};

exports._removeOnWsError = ws => listener => () =>
  ws.removeEventListener("error", listener);

exports._onWsMessage = ws => logger => fn => () => {
  ws.addEventListener("message", function func(event) {
    const str = event.data;
    logger(`message: ${str}`)();
    fn(str)();
  });
};

exports._wsSend = ws => logger => str => () => {
  logger(`sending: ${str}`)();
  ws.send(str);
};

exports._wsReconnect = ws => () => {
  ws.reconnect();
};

exports._wsClose = ws => () => ws.close();
