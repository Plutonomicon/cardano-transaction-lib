/* global BROWSER_RUNTIME */

exports._getAddress = conn => () =>
  conn.getUsedAddresses().then(addrs => addrs[0]);

exports._getCollateral = maybe => conn => () =>
  conn.experimental.getCollateral().then(utxos => {
    return utxos.length ? maybe.just(utxos[0]) : maybe.nothing;
  });

exports._signTx = txHex => conn => () => {
  return conn.signTx(txHex, true).catch(e => {
    console.log("Error in signTx: ", e);
    throw JSON.stringify(e);
  });
};

exports._getBalance = conn => () => conn.getBalance();
