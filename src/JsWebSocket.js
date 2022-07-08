/* global require exports WebSocket BROWSER_RUNTIME */

const ReconnectingWebSocket = require("reconnecting-websocket");

var OurWebSocket;
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

// _mkWebsocket :: (String -> Effect Unit) -> String -> Effect WebSocket
exports._mkWebSocket = (logger) => (url) => () => {
  try {
    var ws;
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

// _onWsConnect :: WebSocket -> (Unit -> Effect Unit) -> Effect Unit
exports._onWsConnect = (ws) => (fn) => () => ws.addEventListener("open", fn);

// _onWsError
//   :: WebSocket
//   -> (String -> Effect Unit) -- logger
//   -> (String -> Effect Unit) -- handler
//   -> Effect ListenerRef
exports._onWsError = (ws) => (logger) => (fn) => () => {
  const listener = function (event) {
    const str = event.toString();
    logger(`WebSocket error: ${str}`)();
    fn(str)();
  };
  ws.addEventListener("error", listener);
  return listener;
};

// _removeOnWsError
//  :: JsWebSocket
//  -> ListenerRef
//  -> Effect Unit
exports._removeOnWsError = (ws) => (listener) => () =>
  ws.removeEventListener("error", listener);

// _onWsMessage
//   :: WebSocket
//   -> (String -> Effect Unit) -- logger
//   -> (String -> Effect Unit) -- handler
//   -> Effect Unit
exports._onWsMessage = (ws) => (logger) => (fn) => () => {
  ws.addEventListener("message", function func(event) {
    const str = event.data;
    logger(`message: ${str}`)();
    fn(str)();
  });
};

// _wsSend :: WebSocket -> (String -> Effect Unit) -> String -> Effect Unit
exports._wsSend = (ws) => (logger) => (str) => () => {
  logger(`sending: ${str}`)();
  ws.send(str);
};

exports._wsReconnect = (ws) => () => {
  ws.reconnect();
};

// _wsClose :: WebSocket -> Effect Unit
exports._wsClose = (ws) => () => ws.close();

// Every 30 seconds if we haven't heard from the server, sever the connection.
// heartbeat
//   :: WebSocket
//   -> (String -> Effect Unit)
//   -> Int
//   -> Effect Unit
//   -> ImplicitUnsafeEffect Int
const heartbeat = (ws) => (logger) => (id) => (onError) => {
  console.log("websocket heartbeat fired")();
  ws.ping();
  if (id !== null) {
    clearTimeout(id);
  }
  const cancelId = setTimeout(() => {
    ws.terminate();
    onError();
  }, 30000);
  return cancelId;
};

// _wsWatch
//   :: WebSocket
//   -> (String -> Effect Unit)
//   -> Effect Unit
//   -> Effect Unit
exports._wsWatch = (ws) => (logger) => (onError) => () => {
  let counter = null;
  let heartbeatAndCount = () => {
    counter = heartbeat(ws, logger, counter, onError);
  };

  ws.addEventListener("open", heartbeatAndCount);
  ws.addEventListener("ping", heartbeatAndCount);
  ws.addEventListener("pong", heartbeatAndCount);
  ws.addEventListener("close", () => clearTimeout(counter));
};
