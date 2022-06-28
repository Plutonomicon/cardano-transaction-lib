/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@emurgo/cardano-serialization-lib-browser');
} else {
    lib = require('@emurgo/cardano-serialization-lib-nodejs');
}

// _getAddress :: Cip30Connection -> Effect (Promise String)
exports._getAddress = conn => () =>
  conn.getUsedAddresses().then((addrs) => addrs[0]);

// _getCollateral
//   :: MaybeFfiHelper
//   -> Cip30Connection
//   -> Effect (Promise String)
exports._getCollateral = maybe => conn => () =>
  conn.experimental.getCollateral().then((utxos) => {
  return utxos.length ? maybe.just(utxos[0]) : maybe.nothing;
});

// _signTx :: String -> Cip30Connection -> Effect (Promise String)
exports._signTx = txHex => conn => () => {
  return conn.signTx(txHex, true)
      .catch(e => {
          console.log("Error in signTx: ", e);
          throw (JSON.stringify(e));
      });
};

// _getBalance :: Cip30Connection -> Effect (Promise string)
exports._getBalance = conn => () => conn.getBalance();
