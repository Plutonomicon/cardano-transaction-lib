const WebSocket = require("ws");

// _mkWebsocket :: String -> Effect WebSocket
exports._mkWebSocket = url => () => {
  console.log("Starting websocket attempt");
  const ws = new WebSocket(url, { perMessageDeflate: false });
  console.log("new websocket");
  return ws;
}

// _onWsConnect :: WebSocket -> (Unit -> Effect Unit) -> Effect Unit
exports._onWsConnect = ws => fn => () => ws.on('open', fn);

// _onWsError :: WebSocket -> (String -> Effect Unit) -> Effect Unit
exports._onWsError = ws => fn => () => { 
  ws.on('error', function func(msg) {
    const str = msg.toString();
    console.log("error: ", msg.toString())
    fn(str)();
  })
}

// _onWsMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit
exports._onWsMessage = ws => fn => () => { 
  ws.on('message', function func(msg) {
    const str = msg.toString();
    console.log("message: ", msg.toString())
    fn(str)();
  })
}

// _wsSend :: WebSocket -> String -> Effect Unit
exports._wsSend = ws => str => () => {
  console.log("sending: ", str);
  ws.send(str);
}

// _wsClose :: Websocket -> Effect Unit
exports._wsClose = ws => () => ws.close()

// _stringify :: a -> Effect String
exports._stringify = a => () => JSON.stringify(a)

