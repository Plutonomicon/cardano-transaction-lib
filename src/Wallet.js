/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@emurgo/cardano-serialization-lib-browser');
} else {
    lib = require('@emurgo/cardano-serialization-lib-nodejs');
}

// _enableNami :: Effect (Promise Cip30Connection)
exports._enableNami = () =>
    window.cardano.nami.enable();

// _enableGero :: Effect (Promise Cip30Connection)
exports._enableGero = () =>
    window.cardano.gerowallet.enable();

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

// foreign import _attachSignature
//   :: ByteArray
//   -> ByteArray
//   -> Effect (ByteArray)
exports._attachSignature = txBytes => witBytes => () => {
  const tx = lib.Transaction.from_bytes(txBytes);
  const newWits = lib.TransactionWitnessSet.from_bytes(witBytes);
  // .vkeys() may return undefined
  const oldvkeyWits = tx.witness_set().vkeys() || lib.Vkeywitnesses.new();
  const newvkeyWits = newWits.vkeys() || lib.Vkeywitnesses.new();

  // Add old vkeyWits into the new set as well to make multi-sign possible
  for (let i = 0; i < oldvkeyWits.len(); i++) {
    newvkeyWits.add(oldvkeyWits.get(i));
  }

  const wits = tx.witness_set();
  // If there are no vkeys in either witness set, we don't want to attach
  // empty vkeys to tx witnesses
  // (So in this case oldvkeyWits remain untouched).
  if (newvkeyWits.len() != 0) {
    wits.set_vkeys(newvkeyWits);
  }

  return lib.Transaction.new(tx.body(), wits, tx.auxiliary_data()).to_bytes();
};
