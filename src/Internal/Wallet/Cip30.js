/* global BROWSER_RUNTIME */

exports._getAddresses = conn => conn.getUsedAddresses;

exports._getCollateral = maybe => conn => () =>
  conn.experimental.getCollateral().then(utxos => {
    return utxos.length ? maybe.just(utxos) : maybe.nothing;
  });

exports._signTx = txHex => conn => () => {
  return conn.signTx(txHex, true).catch(e => {
    console.log("Error in signTx: ", e);
    throw JSON.stringify(e);
  });
};

exports._getBalance = conn => () => conn.getBalance();

exports._getUtxos = maybe => conn => () =>
  conn.getUtxos().then(res => (res === null ? maybe.nothing : maybe.just(res)));
