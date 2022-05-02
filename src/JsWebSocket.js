/* global require exports WebSocket BROWSER_RUNTIME */

const ReconnectingWebSocket = require('reconnecting-websocket');

var OurWebSocket;
if (typeof BROWSER_RUNTIME == 'undefined' || !BROWSER_RUNTIME) {
  OurWebSocket = require("ws");
} else {
  OurWebSocket = WebSocket;
}

class NoPerMessageDeflateWebSocket extends OurWebSocket {
  constructor (url, protocols, options) {
    options = options || {};
    options.perMessageDeflate = false;
    super(url, protocols, options);
  }
};

// _mkWebsocket :: (String -> Effect Unit) -> String -> Effect WebSocket
exports.mkWebSocket = logger => url => () => {
  logger("Starting websocket attempt")();
  var ws;
  if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    ws = new ReconnectingWebSocket.default(url);
  } else {
    ws = new ReconnectingWebSocket(url, [], {
      WebSocket: NoPerMessageDeflateWebSocket
    });
  }
  logger("new websocket")();
  return ws;
};

// _onWsConnect :: WebSocket -> (Unit -> Effect Unit) -> Effect Unit
exports.onWsConnect = ws => fn => () => {
  ws.addEventListener('open', fn);
};

// _onWsError
//   :: WebSocket
//   -> (String -> Effect Unit) -- logger
//   -> (String -> Effect Unit) -- handler
//   -> Effect Unit
exports.onWsError = ws => logger => fn => () => {
  ws.addEventListener('error', function func(event) {
    const str = event.toString();
    logger(`error: ${str}`)();
    fn(str)();
  });
};

// _onWsMessage
//   :: WebSocket
//   -> (String -> Effect Unit) -- logger
//   -> (String -> Effect Unit) -- handler
//   -> Effect Unit
exports.onWsMessage = ws => logger => fn => () => {
  ws.addEventListener('message', function func(event) {
    const str = event.data;
    logger(`message: ${str}`)();
    fn(str)();
  });
};

// _wsSend :: WebSocket -> (String -> Effect Unit) -> String -> Effect Unit
exports.wsSend = ws => logger => str => () => {
  logger(`sending: ${str}`)();
  ws.send(str);
};

// _wsClose :: WebSocket -> Effect Unit
exports.wsClose = ws => () => ws.close();

// Every 30 seconds if we haven't heard from the server, sever the connection.
// heartbeat
//   :: WebSocket
//   -> (String -> Effect Unit)
//   -> Int
//   -> Effect Unit
//   -> ImplicitUnsafeEffect Int
const heartbeat = ws => logger => id => onError => {
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
exports.wsWatch = ws => logger => onError => () => {
  let counter = null;
  let heartbeatAndCount = () => {
    counter = heartbeat(ws, logger, counter, onError);
  };

  ws.addEventListener('open', heartbeatAndCount);
  ws.addEventListener('ping', heartbeatAndCount);
  ws.addEventListener('pong', heartbeatAndCount);
  ws.addEventListener('close', () => clearTimeout(counter));
};
