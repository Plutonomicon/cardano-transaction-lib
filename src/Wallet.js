/* global exports */

// _enableNami :: Effect (Promise NamiConnection)
exports._enableNami = () => window.cardano.nami.enable();

// _getNamiAddress :: NamiConnection -> Effect (Promise String)
exports._getNamiAddress = nami => () =>
  nami.getUsedAddresses().then((addrs) => addrs[0]);

// _getNamiCollateral
//   :: MaybeFfiHelper
//   -> NamiConnection
//   -> Effect (Promise String)
exports._getNamiCollateral = maybe => nami => () =>
  nami.experimental.getCollateral().then((utxos) => {
  return utxos.length ? maybe.just(utxos[0]) : maybe.nothing;
});

// _signTxNami :: String -> NamiConnection -> Effect (Promise String)
exports._signTxNami = txHex => nami => () => nami.signTx(txHex);

// _submitTxNami :: String -> NamiConnection -> Effect (Promise String)
exports._submitTxNami = txHex => nami => () => {
  return nami.submitTx(txHex)
      .catch(e => {
          console.log("Error in submitTxNami: ", e);
          throw (JSON.stringify(e));
      });
};
