/* global require exports WebSocket BROWSER_RUNTIME */

if (typeof BROWSER_RUNTIME == 'undefined' || !BROWSER_RUNTIME) {
    var OurWebSocket = require("ws");
}

// _mkWebsocket :: (String -> Effect Unit) -> String -> Effect WebSocket
exports._mkWebSocket = logger => url => () => {
  logger("Starting websocket attempt")();
  var ws;
  if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    ws = new WebSocket(url);
  } else {
    ws = new OurWebSocket(url, { perMessageDeflate: false });
  }
  logger("new websocket")();
  return ws;
};

// _onWsConnect :: WebSocket -> (Unit -> Effect Unit) -> Effect Unit
exports._onWsConnect = ws => fn => () => {
  ws.addEventListener('open', fn);
};

// _onWsError
//   :: WebSocket
//   -> (String -> Effect Unit) -- logger
//   -> (String -> Effect Unit) -- handler
//   -> Effect Unit
exports._onWsError = ws => logger => fn => () => {
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
exports._onWsMessage = ws => logger => fn => () => {
  ws.addEventListener('message', function func(event) {
    const str = event.data;
    logger(`message: ${str}`)();
    fn(str)();
  });
};

// _wsSend :: WebSocket -> (String -> Effect Unit) -> String -> Effect Unit
exports._wsSend = ws => logger => str => () => {
  logger(`sending: ${str}`)();
  ws.send(str);
};

// _wsClose :: WebSocket -> Effect Unit
exports._wsClose = ws => () => ws.close();

// _stringify :: a -> Effect String
exports._stringify = a => () => JSON.stringify(a);

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
exports._wsWatch = ws => logger => onError => () => {
  let counter = null;
  let heartbeatAndCount = () => {
    counter = heartbeat(ws, loger, counter, onError);
  };

  ws.addEventListener('open', heartbeatAndCount);
  ws.addEventListener('ping', heartbeatAndCount);
  ws.addEventListener('pong', heartbeatAndCount);
  ws.addEventListener('close', () => clearTimeout(counter));
};
