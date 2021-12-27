// const { createInteractionContext, createStateQueryClient } = require('@cardano-ogmios/client')

// getContext = connConfig => () => {
  // return new Promise((res, rej) => {
    // createInteractionContext(
      // err => { console.log("ogmios error: ", err); rej(err)},
      // success => {console.log('getcontext succeeded'); res(success) 
      // }, 
      // connConfig)
  // })
// }

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

// _onWsMessage :: WebSocket -> (data -> Effect Unit) -> Effect Unit
exports._onWsMessage = ws => fn => () => ws.on('message', fn);

// _wsSend :: WebSocket -> String -> Effect Unit
exports._wsSend = ws => str => () => ws.send(str);
